use std::io::Write;
use std::process::{Command, Stdio};

mod util;

pub struct Obj(tempfile::NamedTempFile);

impl Obj {
    fn create() -> Obj {
        Obj(tempfile::NamedTempFile::new_in(".").unwrap())
    }

    fn filename(&self) -> &str {
        self.0.path().file_name().unwrap().to_str().unwrap()
    }

    pub fn from_asm(asm: &str) -> Obj {
        let obj = Obj::create();
        let mut p = invoke_gas(obj.filename());
        let mut stdin = p.stdin.take().unwrap();
        stdin.write_all(b".text\n").unwrap();
        stdin.write_all(asm.as_bytes()).unwrap();
        stdin.write_all(b"\n").unwrap();
        drop(stdin);
        assert!(p.wait().unwrap().success());
        obj
    }

    pub fn from_bytes(bytes: &[u8]) -> Obj {
        let obj = Obj::create();
        let mut p = invoke_gas(obj.filename());
        let mut stdin = p.stdin.take().unwrap();
        stdin.write_all(b".text\n").unwrap();
        for &b in bytes {
            writeln!(stdin, ".byte 0x{:02x}", b).unwrap();
        }
        drop(stdin);
        assert!(p.wait().unwrap().success());
        obj
    }

    pub fn insns(self) -> Vec<Insn> {
        let p = Command::new("bash")
            .args(&["-c", &format!("objdump -d --insn-width=15 {} -M intel", self.filename())])
            .stdout(Stdio::piped())
            .spawn().unwrap();
        let res = p.wait_with_output().unwrap();
        assert!(res.status.success());

        let dump = String::from_utf8(res.stdout).unwrap();
        assert!(!dump.is_empty(), "weird glitch, probably happens with large files");
        parse_objdump(&dump)
    }
}

fn invoke_gas(obj_filename: &str) -> std::process::Child {
    // explicitly calling bash to ensure it also works on Windows with WSL
    Command::new("bash")
        .args(&[
            "-c",
            &format!("as --64 -msyntax=intel -mnaked-reg --strip-local-absolute -o {}", obj_filename),
        ])
        .stdin(Stdio::piped())
        .spawn().unwrap()
}

#[derive(PartialEq, Eq)]
pub struct Insn {
    pub bytes: Vec<u8>,
    pub text: String,
    pub comment: String,
}

impl std::fmt::Debug for Insn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::util::ByteSliceHex;
        let mut d = f.debug_struct("Insn");
        d.field("bytes", &self.bytes.fmt_hex());
        d.field("text", &self.text);
        if !self.comment.is_empty() {
            d.field("comment", &self.comment);
        }
        d.finish()
    }
}

fn parse_objdump(dump: &str) -> Vec<Insn> {
    let lines = dump.lines().skip(7);
    lines.map(|line| {
        let mut it = line.split('\t');
        it.next().unwrap();
        let bytes: Vec<u8> = it.next().unwrap()
            .trim()
            .split(' ')
            .map(|b| u8::from_str_radix(b, 16).unwrap())
            .collect();
        let text_and_comment = it.next().unwrap();
        assert!(it.next().is_none());
        let pos = text_and_comment.find('#').unwrap_or(text_and_comment.len());
        Insn {
            bytes,
            text: text_and_comment[..pos].trim().to_string(),
            comment: text_and_comment[pos..].to_string(),
        }
    }).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn asm() {
        let insns = Obj::from_asm("mov ebx, eax \n nop \n").insns();
        assert_eq!(insns.len(), 2);
        assert_eq!(insns[0].text, "mov    ebx,eax");
        assert_eq!(insns[1].text, "nop");
    }

    #[test]
    fn bytes() {
        let insns = Obj::from_bytes(&[0x89, 0xc3, 0x90]).insns();
        assert_eq!(insns, [
            Insn { bytes: vec![0x89, 0xc3], text: "mov    ebx,eax".to_string(), comment: String::new() },
            Insn { bytes: vec![0x90], text: "nop".to_string(), comment: String::new() },
        ]);
    }
}

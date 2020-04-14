use std::io::Write;
use std::process::{Command, Stdio};

fn invoke_gas(obj_filename: &str) -> std::process::Child {
    // explicitly calling bash to ensure it also works on Windows with WSL
    Command::new("bash")
        .args(&["-c", &format!("as --64 --strip-local-absolute -o {}", obj_filename)])
        .stdin(Stdio::piped())
        .spawn().unwrap()
}

pub fn asm_to_obj(asm: &str, obj_filename: &str) {
    let mut p = invoke_gas(obj_filename);
    let mut stdin = p.stdin.take().unwrap();
    stdin.write_all(b".text\n").unwrap();
    stdin.write_all(asm.as_bytes()).unwrap();
    stdin.write_all(b"\n").unwrap();
    drop(stdin);
    assert!(p.wait().unwrap().success());
}

pub fn bytes_to_obj(bytes: &[u8], obj_filename: &str) {
    let mut p = invoke_gas(obj_filename);
    let mut stdin = p.stdin.take().unwrap();
    stdin.write_all(b".text\n").unwrap();
    for &b in bytes {
        writeln!(stdin, ".byte 0x{:02x}", b).unwrap();
    }
    drop(stdin);
    assert!(p.wait().unwrap().success());
}

fn objdump_raw(obj_filename: &str) -> String {
    let p = Command::new("bash")
        .args(&["-c", &format!("objdump -d {}", obj_filename)])
        .stdout(Stdio::piped())
        .spawn().unwrap();
    let res = p.wait_with_output().unwrap();
    assert!(res.status.success());
    String::from_utf8(res.stdout).unwrap()
}

#[derive(PartialEq)]
pub struct Insn {
    pub bytes: Vec<u8>,
    pub text: String,
}

impl std::fmt::Debug for Insn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::util::ByteSliceHex;
        f.debug_struct("Insn")
            .field("bytes", &self.bytes.fmt_hex())
            .field("text", &self.text)
            .finish()
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
        let text = it.next().unwrap();
        assert!(it.next().is_none());
        Insn {
            bytes,
            text: text.to_string(),
        }
    }).collect()
}

pub fn objdump(obj_filename: &str) -> Vec<Insn> {
    parse_objdump(&objdump_raw(obj_filename))
}

pub fn asm_to_bytes(asm: &str) -> Vec<u8> {
    let tmp = tempfile::NamedTempFile::new_in(".").unwrap();
    let obj_filename = tmp.path().file_name().unwrap().to_str().unwrap();

    asm_to_obj(asm, obj_filename);
    let mut result = vec![];
    for insn in objdump(obj_filename) {
        result.extend_from_slice(&insn.bytes);
    }
    result
}

pub fn bytes_to_asm(bytes: &[u8]) -> String {
    let tmp = tempfile::NamedTempFile::new_in(".").unwrap();
    let obj_filename = tmp.path().file_name().unwrap().to_str().unwrap();

    bytes_to_obj(bytes, obj_filename);
    use crate::util::SepBy;
    format!("{}", objdump(obj_filename).into_iter().map(|insn| insn.text).sep_by("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn asm() {
        let tmp = tempfile::NamedTempFile::new_in(".").unwrap();
        let obj_filename = tmp.path().file_name().unwrap().to_str().unwrap();

        asm_to_obj("mov %eax, %ebx \n nop \n", obj_filename);
        let insns = objdump(obj_filename);
        assert_eq!(insns[0].text, "mov    %eax,%ebx");
        assert_eq!(insns[1].text, "nop");
    }

    #[test]
    fn bytes() {
        let tmp = tempfile::NamedTempFile::new_in(".").unwrap();
        let obj_filename = tmp.path().file_name().unwrap().to_str().unwrap();

        bytes_to_obj(&[0x89, 0xc3, 0x90], obj_filename);
        let insns = objdump(obj_filename);
        assert_eq!(insns, [
            Insn { bytes: vec![0x89, 0xc3], text: "mov    %eax,%ebx".to_string() },
            Insn { bytes: vec![0x90], text: "nop".to_string() },
        ]);
    }

    #[test]
    fn test_asm_to_bytes() {
        assert_eq!(asm_to_bytes("nop"), [0x90]);
    }

    #[test]
    fn test_bytes_to_asm() {
        assert_eq!(bytes_to_asm(&[0x90, 0x90]), "nop\nnop");
    }
}

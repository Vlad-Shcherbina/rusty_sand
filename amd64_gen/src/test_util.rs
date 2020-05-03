use std::fmt::Write;

fn byte_literal(data: &[u8]) -> String {
    let mut s = String::with_capacity(3 + 4 * data.len());
    s.push_str("b\"");
    for &b in data {
        write!(&mut s, "\\x{:02x}", b).unwrap();
    }
    s.push('"');
    assert_eq!(s.capacity(), s.len());
    s
}

fn format_disasm_literal<'a>(disasm: impl Iterator<Item=(&'a [u8], &'a str)>) -> String {
    let disasm: Vec<(String, String)> = disasm.map(|(bytes, text)| {
        (byte_literal(bytes), format!("{:?}", text))
    }).collect();
    let max_bytes_len = disasm.iter().map(|(bytes, _)| bytes.len()).max().unwrap();
    let mut result = String::new();
    result.push_str("----------------- >8 -----\n");
    for (bytes, text) in disasm {
        writeln!(
            &mut result, "    ({},{: <pad$} {}),",
            bytes, "", text,
            pad=max_bytes_len - bytes.len(),
        ).unwrap();
    }
    result.push_str("----- 8< -----------------\n");
    result
}

#[track_caller]
pub fn expect_disasm(code: &[u8], expected_disasm: &[(&[u8], &str)]) {
    let insns = ::binutils::Obj::from_bytes(&code).insns();
    let it0 = insns.iter().map(|i| (i.bytes.as_slice(), i.text.as_str()));
    let mut eq = true;
    let mut it = it0.clone();
    let mut it_e = expected_disasm.iter().copied();
    loop {
        match (it.next(), it_e.next()) {
            (Some((bytes, text)), Some((bytes2, text2))) =>
                if bytes != bytes2 || text != text2 {
                    println!("     got: {:30}   {}", byte_literal(bytes), text);
                    println!("expected: {:30}   {}", byte_literal(bytes2), text2);
                    println!();
                    eq = false;
                }
            (Some((bytes, text)), None) => {
                println!("     got: {:30}   {}", byte_literal(bytes), text);
                println!("expected: nothing");
                println!();
                eq = false;
                break;
            }
            (None, Some((bytes2, text2))) => {
                println!("     got: nothing");
                println!("expected: {:30}   {}", byte_literal(bytes2), text2);
                println!();
                eq = false;
                break;
            }
            (None, None) => break,
        }
    }
    if !eq {
        println!("copy-pasteable result to update the test:");
        print!("{}", format_disasm_literal(it0));
        panic!("disassembly mismatch");
    }
}

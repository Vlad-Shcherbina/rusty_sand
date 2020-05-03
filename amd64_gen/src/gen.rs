use super::*;

// return r, x, b bits of REX prefix
fn encode_modrm(sink: &mut impl CodeSink, r1: Reg, rm: impl Into<RegOrMem2>) -> u8 {
    let rm: RegOrMem2 = rm.into();
    let r1 = r1 as u8;
    let r1_modrm = (r1 & 7) << 3;
    let r1_rex = (r1 & 8) >> 1;
    match rm {
        RegOrMem2::Reg(r2) => {
            let r2 = r2 as u8;
            sink.prepend(&[0b11_000_000 | r1_modrm | (r2 & 7)]);
            r1_rex | r2 >> 3
        }
        RipRel(disp) => {
            sink.prepend(&disp.to_le_bytes());
            sink.prepend(&[0b00_000_101 | r1_modrm]);
            r1_rex
        }
        RegOrMem2::Mem(Mem2 { base, index_scale: None, disp }) => {
            let base = base as u8;
            #[allow(clippy::identity_op)]
            match i8::try_from(disp) {
                Ok(0) if base & 7 != 4 && base & 7 != 5 =>
                    sink.prepend(&[0b00_000_000 | r1_modrm | (base & 7)]),
                Ok(disp) =>
                    if base & 7 != 4 {
                        sink.prepend(&[0b01_000_000 | r1_modrm | (base & 7), disp as u8]);
                    } else {
                        sink.prepend(&[0b01_000_100 | r1_modrm, 0b00_100_100, disp as u8]);
                    }
                Err(_) =>
                    if base & 7 != 4 {
                        sink.prepend(&disp.to_le_bytes());
                        sink.prepend(&[0b10_000_000 | r1_modrm | (base & 7)]);
                    } else {
                        sink.prepend(&disp.to_le_bytes());
                        sink.prepend(&[0b10_000_100 | r1_modrm, 0b00_100_100]);
                    }
            }
            r1_rex | base >> 3
        }
        RegOrMem2::Mem(Mem2 { base, index_scale: Some((index, scale)), disp }) => {
            assert!(index != Reg::Sp);
            let base = base as u8;
            let index = index as u8;
            let scale = match scale {
                1 => 0,
                2 => 1,
                4 => 2,
                8 => 3,
                _ => panic!("{}", scale),
            };
            let sib = scale << 6
                    | (index & 7) << 3
                    | base & 7;
            match i8::try_from(disp) {
                Ok(0) if base & 7 != 5 =>
                    sink.prepend(&[0b00_000_100 | r1_modrm, sib]),
                Ok(disp) => {
                    sink.prepend(&[0b01_000_100 | r1_modrm, sib, disp as u8]);
                }
                Err(_) => {
                    sink.prepend(&disp.to_le_bytes());
                    sink.prepend(&[0b10_000_100 | r1_modrm, sib]);
                }
            }
            r1_rex | base >> 3 | index >> 3 << 1
        }
    }
}

pub fn mov32_r_rm(sink: &mut impl CodeSink, r: Reg, rm: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[0x8B]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn mov64_r_rm(sink: &mut impl CodeSink, r: Reg, rm: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[rex | 0x48, 0x8B]);
}

pub fn mov32_rm_r(sink: &mut impl CodeSink, rm: impl Into<RegOrMem2>, r: Reg) {
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[0x89]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn mov64_rm_r(sink: &mut impl CodeSink, rm: impl Into<RegOrMem2>, r: Reg) {
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[rex | 0x48, 0x89]);
}

pub fn mov32_imm(sink: &mut impl CodeSink, rm: impl Into<RegOrMem2>, imm: i32) {
    sink.prepend(&imm.to_le_bytes());
    let rex = encode_modrm(sink, 0.try_into().unwrap(), rm);
    sink.prepend(&[0xc7]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn mov64_imm(sink: &mut impl CodeSink, rm: impl Into<RegOrMem2>, imm: i32) {
    sink.prepend(&imm.to_le_bytes());
    let rex = encode_modrm(sink, 0.try_into().unwrap(), rm);
    sink.prepend(&[rex | 0x48, 0xc7]);
}

pub fn movabs64_imm(sink: &mut impl CodeSink, r: Reg, imm: i64) {
    let r = r as u8;
    sink.prepend(&imm.to_le_bytes());
    sink.prepend(&[0x48 | r >> 3, 0xb8 | r & 7]);
}

pub fn lea64(sink: &mut impl CodeSink, r: Reg, m: impl Into<RegOrMem2> + Copy) {
    if let RegOrMem2::Reg(_) = m.into() {
        panic!("lea with register");
    }
    let rex = encode_modrm(sink, r, m);
    sink.prepend(&[rex | 0x48, 0x8d]);
}

pub fn binop32_r_rm(sink: &mut impl CodeSink, op: Binop, r: Reg, rm: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[op as u8 * 8 + 3]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn binop64_r_rm(sink: &mut impl CodeSink, op: Binop, r: Reg, rm: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[rex | 0x48, op as u8 * 8 + 3]);
}

pub fn binop32_rm_r(sink: &mut impl CodeSink, op: Binop, rm: impl Into<RegOrMem2>, r: Reg) {
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[op as u8 * 8 + 1]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn binop64_rm_r(sink: &mut impl CodeSink, op: Binop, rm: impl Into<RegOrMem2>, r: Reg) {
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[rex | 0x48, op as u8 * 8 + 1]);
}

pub fn binop32_imm(sink: &mut impl CodeSink, op: Binop, rm: impl Into<RegOrMem2>, imm: i32) {
    sink.prepend(&imm.to_le_bytes());
    let rex = encode_modrm(sink, (op as u8).try_into().unwrap(), rm);
    sink.prepend(&[0x81]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn binop64_imm(sink: &mut impl CodeSink, op: Binop, rm: impl Into<RegOrMem2>, imm: i32) {
    sink.prepend(&imm.to_le_bytes());
    let rex = encode_modrm(sink, (op as u8).try_into().unwrap(), rm);
    sink.prepend(&[rex | 0x48, 0x81]);
}

pub fn mul_op32(sink: &mut impl CodeSink, op: MulOp, rm: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, (op as u8).try_into().unwrap(), rm);
    sink.prepend(&[0xf7]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn mul_op64(sink: &mut impl CodeSink, op: MulOp, rm: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, (op as u8).try_into().unwrap(), rm);
    sink.prepend(&[rex | 0x48, 0xf7]);
}

pub fn push64(sink: &mut impl CodeSink, rm: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, 6.try_into().unwrap(), rm);
    sink.prepend(&[0xff]);
    if rex != 0 {
        // rex.W bit is implied for 64-bit (because 32-bit version is illegal),
        // no need to set it
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn pop64(sink: &mut impl CodeSink, rm: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, 0.try_into().unwrap(), rm);
    sink.prepend(&[0x8f]);
    if rex != 0 {
        // rex.W bit is implied for 64-bit (because 32-bit version is illegal),
        // no need to set it
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn jmp_cond(sink: &mut impl CodeSink, cond: Cond, rel: impl Into<RelJumpTarget>) {
    let cond = cond as u8;
    match rel.into() {
        RelJumpTarget::Rel8(rel) => {
            sink.prepend(&[0x70 | cond, rel as u8]);
        }
        RelJumpTarget::Rel32(rel) => {
            sink.prepend(&rel.to_le_bytes());
            sink.prepend(&[0x0f, 0x80 | cond]);
        }
    }
}

pub fn jmp_rel(sink: &mut impl CodeSink, rel: impl Into<RelJumpTarget>) {
    match rel.into() {
        RelJumpTarget::Rel8(rel) => {
            sink.prepend(&[0xeb, rel as u8]);
        }
        RelJumpTarget::Rel32(rel) => {
            sink.prepend(&rel.to_le_bytes());
            sink.prepend(&[0xe9]);
        }
    }
}

pub fn call_rel(sink: &mut impl CodeSink, rel: i32) {
    sink.prepend(&rel.to_le_bytes());
    sink.prepend(&[0xe8]);
}

pub fn jmp_indirect(sink: &mut impl CodeSink, target: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, 4.try_into().unwrap(), target);
    sink.prepend(&[0xff]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn call_indirect(sink: &mut impl CodeSink, target: impl Into<RegOrMem2>) {
    let rex = encode_modrm(sink, 2.try_into().unwrap(), target);
    sink.prepend(&[0xff]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn ret(sink: &mut impl CodeSink) {
    sink.prepend(&[0xc3]);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::expect_disasm;

    impl CodeSink for Vec<u8> {
        fn prepend(&mut self, data: &[u8]) {
            self.splice(0..0, data.iter().copied());
        }
    }

    #[test]
    fn reg_names32() {
        let mut code = Vec::<u8>::new();
        for r in Reg::all() {
            gen::mov32_r_rm(&mut code, r, r);
        }
        let expected: &[(&[u8], &str)] = &[
            (b"\x45\x8b\xff", "mov    r15d,r15d"),
            (b"\x45\x8b\xf6", "mov    r14d,r14d"),
            (b"\x45\x8b\xed", "mov    r13d,r13d"),
            (b"\x45\x8b\xe4", "mov    r12d,r12d"),
            (b"\x45\x8b\xdb", "mov    r11d,r11d"),
            (b"\x45\x8b\xd2", "mov    r10d,r10d"),
            (b"\x45\x8b\xc9", "mov    r9d,r9d"),
            (b"\x45\x8b\xc0", "mov    r8d,r8d"),
            (b"\x8b\xff",     "mov    edi,edi"),
            (b"\x8b\xf6",     "mov    esi,esi"),
            (b"\x8b\xed",     "mov    ebp,ebp"),
            (b"\x8b\xe4",     "mov    esp,esp"),
            (b"\x8b\xdb",     "mov    ebx,ebx"),
            (b"\x8b\xd2",     "mov    edx,edx"),
            (b"\x8b\xc9",     "mov    ecx,ecx"),
            (b"\x8b\xc0",     "mov    eax,eax"),
        ];
        expect_disasm(&code, expected);
        for (r, &(_, text)) in Reg::all().zip(expected.iter().rev()) {
            assert_eq!(text, format!("mov    {},{}", r.name32(), r.name32()));
        }
    }

    #[test]
    fn reg_names64() {
        let mut code = Vec::<u8>::new();
        for r in Reg::all() {
            gen::mov64_r_rm(&mut code, r, r);
        }
        let expected: &[(&[u8], &str)] = &[
            (b"\x4d\x8b\xff", "mov    r15,r15"),
            (b"\x4d\x8b\xf6", "mov    r14,r14"),
            (b"\x4d\x8b\xed", "mov    r13,r13"),
            (b"\x4d\x8b\xe4", "mov    r12,r12"),
            (b"\x4d\x8b\xdb", "mov    r11,r11"),
            (b"\x4d\x8b\xd2", "mov    r10,r10"),
            (b"\x4d\x8b\xc9", "mov    r9,r9"),
            (b"\x4d\x8b\xc0", "mov    r8,r8"),
            (b"\x48\x8b\xff", "mov    rdi,rdi"),
            (b"\x48\x8b\xf6", "mov    rsi,rsi"),
            (b"\x48\x8b\xed", "mov    rbp,rbp"),
            (b"\x48\x8b\xe4", "mov    rsp,rsp"),
            (b"\x48\x8b\xdb", "mov    rbx,rbx"),
            (b"\x48\x8b\xd2", "mov    rdx,rdx"),
            (b"\x48\x8b\xc9", "mov    rcx,rcx"),
            (b"\x48\x8b\xc0", "mov    rax,rax"),
        ];
        expect_disasm(&code, expected);
        for (r, &(_, text)) in Reg::all().zip(expected.iter().rev()) {
            assert_eq!(text, format!("mov    {},{}", r.name64(), r.name64()));
        }
    }

    #[test]
    fn mov_r_r() {
        let mut code = Vec::<u8>::new();
        for &r1 in &[Reg::Cx, Reg::R15] {
            for &r2 in &[Reg::Bx, Reg::R15] {
                gen::mov32_r_rm(&mut code, r1, r2);
                gen::mov32_rm_r(&mut code, r1, r2);
            }
        }
        expect_disasm(&code, &[
            (b"\x45\x89\xff", "mov    r15d,r15d"),
            (b"\x45\x8b\xff", "mov    r15d,r15d"),
            (b"\x41\x89\xdf", "mov    r15d,ebx"),
            (b"\x44\x8b\xfb", "mov    r15d,ebx"),
            (b"\x44\x89\xf9", "mov    ecx,r15d"),
            (b"\x41\x8b\xcf", "mov    ecx,r15d"),
            (b"\x89\xd9",     "mov    ecx,ebx"),
            (b"\x8b\xcb",     "mov    ecx,ebx"),
        ]);

        let mut code = Vec::<u8>::new();
        for &r1 in &[Reg::Cx, Reg::R15] {
            for &r2 in &[Reg::Bx, Reg::R15] {
                gen::mov64_r_rm(&mut code, r1, r2);
                gen::mov64_rm_r(&mut code, r1, r2);
            }
        }
        expect_disasm(&code, &[
            (b"\x4d\x89\xff", "mov    r15,r15"),
            (b"\x4d\x8b\xff", "mov    r15,r15"),
            (b"\x49\x89\xdf", "mov    r15,rbx"),
            (b"\x4c\x8b\xfb", "mov    r15,rbx"),
            (b"\x4c\x89\xf9", "mov    rcx,r15"),
            (b"\x49\x8b\xcf", "mov    rcx,r15"),
            (b"\x48\x89\xd9", "mov    rcx,rbx"),
            (b"\x48\x8b\xcb", "mov    rcx,rbx"),
        ]);
    }

    #[test]
    fn mov_rm() {
        let mut code = Vec::<u8>::new();
        gen::mov32_r_rm(&mut code, Reg::Bx, Mem2::base(Reg::Si));
        gen::mov32_rm_r(&mut code, Mem2::base(Reg::R8), Reg::R11);
        expect_disasm(&code, &[
            (b"\x45\x89\x18", "mov    DWORD PTR [r8],r11d"),
            (b"\x8b\x1e",     "mov    ebx,DWORD PTR [rsi]"),
        ]);
    }

    #[test]
    fn mov_imm() {
        let mut code = Vec::<u8>::new();
        gen::mov32_imm(&mut code, Reg::R9, 0x42);
        gen::mov32_imm(&mut code, Mem2::base(Reg::R11), 0x42);
        gen::mov64_imm(&mut code, Reg::Dx, -2);
        expect_disasm(&code, &[
            (b"\x48\xc7\xc2\xfe\xff\xff\xff", "mov    rdx,0xfffffffffffffffe"),
            (b"\x41\xc7\x03\x42\x00\x00\x00", "mov    DWORD PTR [r11],0x42"),
            (b"\x41\xc7\xc1\x42\x00\x00\x00", "mov    r9d,0x42"),
        ]);
    }

    #[test]
    fn movabs() {
        let mut code = Vec::<u8>::new();
        gen::movabs64_imm(&mut code, Reg::Bx, 0x1234567890);
        gen::movabs64_imm(&mut code, Reg::R9, 0x1234567890);
        expect_disasm(&code, &[
            (b"\x49\xb9\x90\x78\x56\x34\x12\x00\x00\x00", "movabs r9,0x1234567890"),
            (b"\x48\xbb\x90\x78\x56\x34\x12\x00\x00\x00", "movabs rbx,0x1234567890"),
        ]);
    }

    #[test]
    fn lea() {
        let mut code = Vec::<u8>::new();
        gen::lea64(&mut code, Reg::Cx, Mem2::base(Reg::R11));
        gen::lea64(&mut code, Reg::Cx, RipRel(0x42));
        expect_disasm(&code, &[
            (b"\x48\x8d\x0d\x42\x00\x00\x00", "lea    rcx,[rip+0x42]"),
            (b"\x49\x8d\x0b",                 "lea    rcx,[r11]"),
        ]);
    }

    #[test]
    fn binop_names() {
        let mut code = Vec::<u8>::new();
        let mut expected_texts = Vec::new();
        for &op in &[
            Binop::Add, Binop::Or, Binop::Adc, Binop::Sbb,
            Binop::And, Binop::Sub, Binop::Xor, Binop::Cmp,
        ] {
            gen::binop32_r_rm(&mut code, op, Reg::Dx, Reg::Ax);
            let mut op_name = format!("{:?}", op);
            op_name.make_ascii_lowercase();
            expected_texts.push(format!("{:3}    edx,eax", op_name));
        }
        let expected: &[(&[u8], &str)] = &[
            (b"\x3b\xd0", "cmp    edx,eax"),
            (b"\x33\xd0", "xor    edx,eax"),
            (b"\x2b\xd0", "sub    edx,eax"),
            (b"\x23\xd0", "and    edx,eax"),
            (b"\x1b\xd0", "sbb    edx,eax"),
            (b"\x13\xd0", "adc    edx,eax"),
            (b"\x0b\xd0", "or     edx,eax"),
            (b"\x03\xd0", "add    edx,eax"),
        ];
        expect_disasm(&code, expected);
        assert_eq!(expected_texts.len(), expected.len());
        for (et, &(_, t)) in expected_texts.iter().rev().zip(expected) {
            assert_eq!(et, t);
        }
    }

    #[test]
    fn binop() {
        let mut code = Vec::<u8>::new();
        gen::binop32_r_rm(&mut code, Binop::Or, Reg::Ax, Reg::Bx);
        gen::binop32_rm_r(&mut code, Binop::Or, Reg::Ax, Reg::Bx);
        gen::binop64_r_rm(&mut code, Binop::Xor, Reg::Ax, Reg::Bx);
        gen::binop64_rm_r(&mut code, Binop::Xor, Reg::Ax, Reg::Bx);
        gen::binop32_imm(&mut code, Binop::Sub, Reg::Cx, 0x42);
        gen::binop64_imm(&mut code, Binop::Sbb, Reg::Cx, 0x42);
        expect_disasm(&code, &[
            (b"\x48\x81\xd9\x42\x00\x00\x00", "sbb    rcx,0x42"),
            (b"\x81\xe9\x42\x00\x00\x00",     "sub    ecx,0x42"),
            (b"\x48\x31\xd8",                 "xor    rax,rbx"),
            (b"\x48\x33\xc3",                 "xor    rax,rbx"),
            (b"\x09\xd8",                     "or     eax,ebx"),
            (b"\x0b\xc3",                     "or     eax,ebx"),
        ]);
    }

    #[test]
    fn mul_op_names() {
        let mut code = Vec::<u8>::new();
        let mut expected_texts = Vec::new();
        for &op in &[MulOp::Mul, MulOp::Imul, MulOp::Div, MulOp::Idiv] {
            gen::mul_op32(&mut code, op, Reg::Bx);
            let mut op_name = format!("{:?}", op);
            op_name.make_ascii_lowercase();
            expected_texts.push(format!("{:4}   ebx", op_name));
        }
        let expected: &[(&[u8], &str)] = &[
            (b"\xf7\xfb", "idiv   ebx"),
            (b"\xf7\xf3", "div    ebx"),
            (b"\xf7\xeb", "imul   ebx"),
            (b"\xf7\xe3", "mul    ebx"),
        ];
        expect_disasm(&code, expected);
        assert_eq!(expected_texts.len(), expected.len());
        for (et, &(_, t)) in expected_texts.iter().rev().zip(expected) {
            assert_eq!(et, t);
        }
    }

    #[test]
    fn mul_op() {
        let mut code = Vec::<u8>::new();
        gen::mul_op32(&mut code, MulOp::Imul, Reg::R10);
        gen::mul_op64(&mut code, MulOp::Div, Reg::R10);
        expect_disasm(&code, &[
            (b"\x49\xf7\xf2", "div    r10"),
            (b"\x41\xf7\xea", "imul   r10d"),
        ]);
    }

    #[test]
    fn push_pop() {
        let mut code = Vec::<u8>::new();
        gen::push64(&mut code, Reg::Cx);
        gen::push64(&mut code, Mem2::base(Reg::R15));
        gen::pop64(&mut code, Mem2::base(Reg::R15));
        gen::pop64(&mut code, Reg::Cx);
        expect_disasm(&code, &[
            (b"\x8f\xc1",     "pop    rcx"),
            (b"\x41\x8f\x07", "pop    QWORD PTR [r15]"),
            (b"\x41\xff\x37", "push   QWORD PTR [r15]"),
            (b"\xff\xf1",     "push   rcx"),
        ]);
    }

    #[test]
    fn jmp_cond() {
        let mut code = Vec::<u8>::new();
        let mut expected_starts = Vec::new();
        for cond in Cond::all() {
            for &rel in &[5, 1000] {
                gen::jmp_cond(&mut code, cond, rel);
                let mut cond_name = format!("j{:?} ", cond);
                cond_name.make_ascii_lowercase();
                expected_starts.push(cond_name);
            }
        }
        let expected: &[(&[u8], &str)] = &[
            (b"\x0f\x8f\xe8\x03\x00\x00", "jg     0x3ee"),
            (b"\x7f\x05",                 "jg     0xd"),
            (b"\x0f\x8e\xe8\x03\x00\x00", "jle    0x3f6"),
            (b"\x7e\x05",                 "jle    0x15"),
            (b"\x0f\x8d\xe8\x03\x00\x00", "jge    0x3fe"),
            (b"\x7d\x05",                 "jge    0x1d"),
            (b"\x0f\x8c\xe8\x03\x00\x00", "jl     0x406"),
            (b"\x7c\x05",                 "jl     0x25"),
            (b"\x0f\x8b\xe8\x03\x00\x00", "jnp    0x40e"),
            (b"\x7b\x05",                 "jnp    0x2d"),
            (b"\x0f\x8a\xe8\x03\x00\x00", "jp     0x416"),
            (b"\x7a\x05",                 "jp     0x35"),
            (b"\x0f\x89\xe8\x03\x00\x00", "jns    0x41e"),
            (b"\x79\x05",                 "jns    0x3d"),
            (b"\x0f\x88\xe8\x03\x00\x00", "js     0x426"),
            (b"\x78\x05",                 "js     0x45"),
            (b"\x0f\x87\xe8\x03\x00\x00", "ja     0x42e"),
            (b"\x77\x05",                 "ja     0x4d"),
            (b"\x0f\x86\xe8\x03\x00\x00", "jbe    0x436"),
            (b"\x76\x05",                 "jbe    0x55"),
            (b"\x0f\x85\xe8\x03\x00\x00", "jne    0x43e"),
            (b"\x75\x05",                 "jne    0x5d"),
            (b"\x0f\x84\xe8\x03\x00\x00", "je     0x446"),
            (b"\x74\x05",                 "je     0x65"),
            (b"\x0f\x83\xe8\x03\x00\x00", "jae    0x44e"),
            (b"\x73\x05",                 "jae    0x6d"),
            (b"\x0f\x82\xe8\x03\x00\x00", "jb     0x456"),
            (b"\x72\x05",                 "jb     0x75"),
            (b"\x0f\x81\xe8\x03\x00\x00", "jno    0x45e"),
            (b"\x71\x05",                 "jno    0x7d"),
            (b"\x0f\x80\xe8\x03\x00\x00", "jo     0x466"),
            (b"\x70\x05",                 "jo     0x85"),
        ];
        expect_disasm(&code, expected);
        assert_eq!(expected_starts.len(), expected.len());
        for (es, &(_, t)) in expected_starts.iter().rev().zip(expected) {
            assert!(t.starts_with(es));
        }
    }

    #[test]
    fn jmp_call_rel() {
        let mut code = Vec::<u8>::new();
        gen::jmp_rel(&mut code, -2);
        gen::jmp_rel(&mut code, 1000);
        gen::jmp_rel(&mut code, RelJumpTarget::Rel32(-5));
        gen::call_rel(&mut code, -5);
        expect_disasm(&code, &[
            (b"\xe8\xfb\xff\xff\xff", "call   0x0"),
            (b"\xe9\xfb\xff\xff\xff", "jmp    0x5"),
            (b"\xe9\xe8\x03\x00\x00", "jmp    0x3f7"),
            (b"\xeb\xfe",             "jmp    0xf"),
        ]);
    }

    #[test]
    fn jmp_call_indirect() {
        let mut code = Vec::<u8>::new();
        gen::jmp_indirect(&mut code, Reg::Cx);
        gen::call_indirect(&mut code, Mem2::base(Reg::R14));
        expect_disasm(&code, &[
            (b"\x41\xff\x16", "call   QWORD PTR [r14]"),
            (b"\xff\xe1",     "jmp    rcx"),
        ]);
    }

    #[test]
    fn ret() {
        let mut code = Vec::<u8>::new();
        gen::ret(&mut code);
        expect_disasm(&code, &[
            (b"\xc3", "ret"),
        ]);
    }

    #[test]
    fn rip_rel() {
        let mut code = Vec::<u8>::new();
        gen::mov32_r_rm(&mut code, Reg::Ax, RipRel(2));
        gen::mov32_r_rm(&mut code, Reg::Bx, RipRel(-2));
        expect_disasm(&code, &[
            (b"\x8b\x1d\xfe\xff\xff\xff", "mov    ebx,DWORD PTR [rip+0xfffffffffffffffe]"),
            (b"\x8b\x05\x02\x00\x00\x00", "mov    eax,DWORD PTR [rip+0x2]"),        ]);
    }

    #[test]
    fn base_only() {
        let mut code = Vec::<u8>::new();
        for base in Reg::all() {
            gen::mov32_r_rm(&mut code, Reg::Cx, Mem2::base(base));
        }
        expect_disasm(&code, &[
            (b"\x41\x8b\x0f",         "mov    ecx,DWORD PTR [r15]"),
            (b"\x41\x8b\x0e",         "mov    ecx,DWORD PTR [r14]"),
            (b"\x41\x8b\x4d\x00",     "mov    ecx,DWORD PTR [r13+0x0]"),
            (b"\x41\x8b\x4c\x24\x00", "mov    ecx,DWORD PTR [r12+0x0]"),
            (b"\x41\x8b\x0b",         "mov    ecx,DWORD PTR [r11]"),
            (b"\x41\x8b\x0a",         "mov    ecx,DWORD PTR [r10]"),
            (b"\x41\x8b\x09",         "mov    ecx,DWORD PTR [r9]"),
            (b"\x41\x8b\x08",         "mov    ecx,DWORD PTR [r8]"),
            (b"\x8b\x0f",             "mov    ecx,DWORD PTR [rdi]"),
            (b"\x8b\x0e",             "mov    ecx,DWORD PTR [rsi]"),
            (b"\x8b\x4d\x00",         "mov    ecx,DWORD PTR [rbp+0x0]"),
            (b"\x8b\x4c\x24\x00",     "mov    ecx,DWORD PTR [rsp+0x0]"),
            (b"\x8b\x0b",             "mov    ecx,DWORD PTR [rbx]"),
            (b"\x8b\x0a",             "mov    ecx,DWORD PTR [rdx]"),
            (b"\x8b\x09",             "mov    ecx,DWORD PTR [rcx]"),
            (b"\x8b\x08",             "mov    ecx,DWORD PTR [rax]"),
        ]);
    }

    #[test]
    fn base_only_disp8() {
        let mut code = Vec::<u8>::new();
        for base in Reg::all() {
            gen::mov32_r_rm(&mut code, Reg::Cx, Mem2::base(base).disp(5));
        }
        expect_disasm(&code, &[
            (b"\x41\x8b\x4f\x05",     "mov    ecx,DWORD PTR [r15+0x5]"),
            (b"\x41\x8b\x4e\x05",     "mov    ecx,DWORD PTR [r14+0x5]"),
            (b"\x41\x8b\x4d\x05",     "mov    ecx,DWORD PTR [r13+0x5]"),
            (b"\x41\x8b\x4c\x24\x05", "mov    ecx,DWORD PTR [r12+0x5]"),
            (b"\x41\x8b\x4b\x05",     "mov    ecx,DWORD PTR [r11+0x5]"),
            (b"\x41\x8b\x4a\x05",     "mov    ecx,DWORD PTR [r10+0x5]"),
            (b"\x41\x8b\x49\x05",     "mov    ecx,DWORD PTR [r9+0x5]"),
            (b"\x41\x8b\x48\x05",     "mov    ecx,DWORD PTR [r8+0x5]"),
            (b"\x8b\x4f\x05",         "mov    ecx,DWORD PTR [rdi+0x5]"),
            (b"\x8b\x4e\x05",         "mov    ecx,DWORD PTR [rsi+0x5]"),
            (b"\x8b\x4d\x05",         "mov    ecx,DWORD PTR [rbp+0x5]"),
            (b"\x8b\x4c\x24\x05",     "mov    ecx,DWORD PTR [rsp+0x5]"),
            (b"\x8b\x4b\x05",         "mov    ecx,DWORD PTR [rbx+0x5]"),
            (b"\x8b\x4a\x05",         "mov    ecx,DWORD PTR [rdx+0x5]"),
            (b"\x8b\x49\x05",         "mov    ecx,DWORD PTR [rcx+0x5]"),
            (b"\x8b\x48\x05",         "mov    ecx,DWORD PTR [rax+0x5]"),
        ]);
    }

    #[test]
    fn base_only_disp32() {
        let mut code = Vec::<u8>::new();
        for base in Reg::all() {
            gen::mov32_r_rm(&mut code, Reg::Cx, Mem2::base(base).disp(0x333));
        }
        expect_disasm(&code, &[
            (b"\x41\x8b\x8f\x33\x03\x00\x00",     "mov    ecx,DWORD PTR [r15+0x333]"),
            (b"\x41\x8b\x8e\x33\x03\x00\x00",     "mov    ecx,DWORD PTR [r14+0x333]"),
            (b"\x41\x8b\x8d\x33\x03\x00\x00",     "mov    ecx,DWORD PTR [r13+0x333]"),
            (b"\x41\x8b\x8c\x24\x33\x03\x00\x00", "mov    ecx,DWORD PTR [r12+0x333]"),
            (b"\x41\x8b\x8b\x33\x03\x00\x00",     "mov    ecx,DWORD PTR [r11+0x333]"),
            (b"\x41\x8b\x8a\x33\x03\x00\x00",     "mov    ecx,DWORD PTR [r10+0x333]"),
            (b"\x41\x8b\x89\x33\x03\x00\x00",     "mov    ecx,DWORD PTR [r9+0x333]"),
            (b"\x41\x8b\x88\x33\x03\x00\x00",     "mov    ecx,DWORD PTR [r8+0x333]"),
            (b"\x8b\x8f\x33\x03\x00\x00",         "mov    ecx,DWORD PTR [rdi+0x333]"),
            (b"\x8b\x8e\x33\x03\x00\x00",         "mov    ecx,DWORD PTR [rsi+0x333]"),
            (b"\x8b\x8d\x33\x03\x00\x00",         "mov    ecx,DWORD PTR [rbp+0x333]"),
            (b"\x8b\x8c\x24\x33\x03\x00\x00",     "mov    ecx,DWORD PTR [rsp+0x333]"),
            (b"\x8b\x8b\x33\x03\x00\x00",         "mov    ecx,DWORD PTR [rbx+0x333]"),
            (b"\x8b\x8a\x33\x03\x00\x00",         "mov    ecx,DWORD PTR [rdx+0x333]"),
            (b"\x8b\x89\x33\x03\x00\x00",         "mov    ecx,DWORD PTR [rcx+0x333]"),
            (b"\x8b\x88\x33\x03\x00\x00",         "mov    ecx,DWORD PTR [rax+0x333]"),
        ]);
    }

    #[test]
    fn sib_scales() {
        let mut code = Vec::<u8>::new();
        for &scale in &[1, 2, 4, 8] {
            gen::mov32_r_rm(&mut code, Reg::Cx, Mem2::base(Reg::Bx).index_scale(Reg::Ax, scale));
        }
        expect_disasm(&code, &[
            (b"\x8b\x0c\xc3", "mov    ecx,DWORD PTR [rbx+rax*8]"),
            (b"\x8b\x0c\x83", "mov    ecx,DWORD PTR [rbx+rax*4]"),
            (b"\x8b\x0c\x43", "mov    ecx,DWORD PTR [rbx+rax*2]"),
            (b"\x8b\x0c\x03", "mov    ecx,DWORD PTR [rbx+rax*1]"),
        ]);
    }

    #[test]
    fn sib_combinations() {
        let bases = [Reg::Ax, Reg::Sp, Reg::Bp, Reg::R12, Reg::R13, Reg::R15];
        let indexes = [Reg::Ax, Reg::Bp, Reg::R12, Reg::R13, Reg::R15];
        let mut code = Vec::<u8>::new();
        let mut expected_texts = Vec::new();
        for &base in &bases {
            for &index in &indexes {
                gen::mov32_r_rm(&mut code, Reg::Bx, Mem2::base(base).index_scale(index, 2));
                expected_texts.push(
                    format!("mov    ebx,DWORD PTR [{}+{}*2]", base.name64(), index.name64()));
            }
        }
        let expected: &[(&[u8], &str)] = &[
            (b"\x43\x8b\x1c\x7f",     "mov    ebx,DWORD PTR [r15+r15*2]"),
            (b"\x43\x8b\x1c\x6f",     "mov    ebx,DWORD PTR [r15+r13*2]"),
            (b"\x43\x8b\x1c\x67",     "mov    ebx,DWORD PTR [r15+r12*2]"),
            (b"\x41\x8b\x1c\x6f",     "mov    ebx,DWORD PTR [r15+rbp*2]"),
            (b"\x41\x8b\x1c\x47",     "mov    ebx,DWORD PTR [r15+rax*2]"),
            (b"\x43\x8b\x5c\x7d\x00", "mov    ebx,DWORD PTR [r13+r15*2+0x0]"),
            (b"\x43\x8b\x5c\x6d\x00", "mov    ebx,DWORD PTR [r13+r13*2+0x0]"),
            (b"\x43\x8b\x5c\x65\x00", "mov    ebx,DWORD PTR [r13+r12*2+0x0]"),
            (b"\x41\x8b\x5c\x6d\x00", "mov    ebx,DWORD PTR [r13+rbp*2+0x0]"),
            (b"\x41\x8b\x5c\x45\x00", "mov    ebx,DWORD PTR [r13+rax*2+0x0]"),
            (b"\x43\x8b\x1c\x7c",     "mov    ebx,DWORD PTR [r12+r15*2]"),
            (b"\x43\x8b\x1c\x6c",     "mov    ebx,DWORD PTR [r12+r13*2]"),
            (b"\x43\x8b\x1c\x64",     "mov    ebx,DWORD PTR [r12+r12*2]"),
            (b"\x41\x8b\x1c\x6c",     "mov    ebx,DWORD PTR [r12+rbp*2]"),
            (b"\x41\x8b\x1c\x44",     "mov    ebx,DWORD PTR [r12+rax*2]"),
            (b"\x42\x8b\x5c\x7d\x00", "mov    ebx,DWORD PTR [rbp+r15*2+0x0]"),
            (b"\x42\x8b\x5c\x6d\x00", "mov    ebx,DWORD PTR [rbp+r13*2+0x0]"),
            (b"\x42\x8b\x5c\x65\x00", "mov    ebx,DWORD PTR [rbp+r12*2+0x0]"),
            (b"\x8b\x5c\x6d\x00",     "mov    ebx,DWORD PTR [rbp+rbp*2+0x0]"),
            (b"\x8b\x5c\x45\x00",     "mov    ebx,DWORD PTR [rbp+rax*2+0x0]"),
            (b"\x42\x8b\x1c\x7c",     "mov    ebx,DWORD PTR [rsp+r15*2]"),
            (b"\x42\x8b\x1c\x6c",     "mov    ebx,DWORD PTR [rsp+r13*2]"),
            (b"\x42\x8b\x1c\x64",     "mov    ebx,DWORD PTR [rsp+r12*2]"),
            (b"\x8b\x1c\x6c",         "mov    ebx,DWORD PTR [rsp+rbp*2]"),
            (b"\x8b\x1c\x44",         "mov    ebx,DWORD PTR [rsp+rax*2]"),
            (b"\x42\x8b\x1c\x78",     "mov    ebx,DWORD PTR [rax+r15*2]"),
            (b"\x42\x8b\x1c\x68",     "mov    ebx,DWORD PTR [rax+r13*2]"),
            (b"\x42\x8b\x1c\x60",     "mov    ebx,DWORD PTR [rax+r12*2]"),
            (b"\x8b\x1c\x68",         "mov    ebx,DWORD PTR [rax+rbp*2]"),
            (b"\x8b\x1c\x40",         "mov    ebx,DWORD PTR [rax+rax*2]"),
        ];
        expect_disasm(&code, expected);
        assert_eq!(expected_texts.len(), expected.len());
        for (et, &(_, t)) in expected_texts.iter().rev().zip(expected) {
            assert_eq!(et, &t.replace("+0x0]", "]"));
        }
    }

    #[test]
    fn sib_combinations_disp8() {
        let bases = [Reg::Ax, Reg::Sp, Reg::Bp, Reg::R12, Reg::R13, Reg::R15];
        let indexes = [Reg::Ax, Reg::Bp, Reg::R12, Reg::R13, Reg::R15];
        let mut code = Vec::<u8>::new();
        let mut expected_texts = Vec::new();
        for &base in &bases {
            for &index in &indexes {
                gen::mov32_r_rm(&mut code, Reg::Bx, Mem2::base(base).index_scale(index, 2).disp(-5));
                expected_texts.push(
                    format!("mov    ebx,DWORD PTR [{}+{}*2-0x5]", base.name64(), index.name64()));
            }
        }
        let expected: &[(&[u8], &str)] = &[
            (b"\x43\x8b\x5c\x7f\xfb", "mov    ebx,DWORD PTR [r15+r15*2-0x5]"),
            (b"\x43\x8b\x5c\x6f\xfb", "mov    ebx,DWORD PTR [r15+r13*2-0x5]"),
            (b"\x43\x8b\x5c\x67\xfb", "mov    ebx,DWORD PTR [r15+r12*2-0x5]"),
            (b"\x41\x8b\x5c\x6f\xfb", "mov    ebx,DWORD PTR [r15+rbp*2-0x5]"),
            (b"\x41\x8b\x5c\x47\xfb", "mov    ebx,DWORD PTR [r15+rax*2-0x5]"),
            (b"\x43\x8b\x5c\x7d\xfb", "mov    ebx,DWORD PTR [r13+r15*2-0x5]"),
            (b"\x43\x8b\x5c\x6d\xfb", "mov    ebx,DWORD PTR [r13+r13*2-0x5]"),
            (b"\x43\x8b\x5c\x65\xfb", "mov    ebx,DWORD PTR [r13+r12*2-0x5]"),
            (b"\x41\x8b\x5c\x6d\xfb", "mov    ebx,DWORD PTR [r13+rbp*2-0x5]"),
            (b"\x41\x8b\x5c\x45\xfb", "mov    ebx,DWORD PTR [r13+rax*2-0x5]"),
            (b"\x43\x8b\x5c\x7c\xfb", "mov    ebx,DWORD PTR [r12+r15*2-0x5]"),
            (b"\x43\x8b\x5c\x6c\xfb", "mov    ebx,DWORD PTR [r12+r13*2-0x5]"),
            (b"\x43\x8b\x5c\x64\xfb", "mov    ebx,DWORD PTR [r12+r12*2-0x5]"),
            (b"\x41\x8b\x5c\x6c\xfb", "mov    ebx,DWORD PTR [r12+rbp*2-0x5]"),
            (b"\x41\x8b\x5c\x44\xfb", "mov    ebx,DWORD PTR [r12+rax*2-0x5]"),
            (b"\x42\x8b\x5c\x7d\xfb", "mov    ebx,DWORD PTR [rbp+r15*2-0x5]"),
            (b"\x42\x8b\x5c\x6d\xfb", "mov    ebx,DWORD PTR [rbp+r13*2-0x5]"),
            (b"\x42\x8b\x5c\x65\xfb", "mov    ebx,DWORD PTR [rbp+r12*2-0x5]"),
            (b"\x8b\x5c\x6d\xfb",     "mov    ebx,DWORD PTR [rbp+rbp*2-0x5]"),
            (b"\x8b\x5c\x45\xfb",     "mov    ebx,DWORD PTR [rbp+rax*2-0x5]"),
            (b"\x42\x8b\x5c\x7c\xfb", "mov    ebx,DWORD PTR [rsp+r15*2-0x5]"),
            (b"\x42\x8b\x5c\x6c\xfb", "mov    ebx,DWORD PTR [rsp+r13*2-0x5]"),
            (b"\x42\x8b\x5c\x64\xfb", "mov    ebx,DWORD PTR [rsp+r12*2-0x5]"),
            (b"\x8b\x5c\x6c\xfb",     "mov    ebx,DWORD PTR [rsp+rbp*2-0x5]"),
            (b"\x8b\x5c\x44\xfb",     "mov    ebx,DWORD PTR [rsp+rax*2-0x5]"),
            (b"\x42\x8b\x5c\x78\xfb", "mov    ebx,DWORD PTR [rax+r15*2-0x5]"),
            (b"\x42\x8b\x5c\x68\xfb", "mov    ebx,DWORD PTR [rax+r13*2-0x5]"),
            (b"\x42\x8b\x5c\x60\xfb", "mov    ebx,DWORD PTR [rax+r12*2-0x5]"),
            (b"\x8b\x5c\x68\xfb",     "mov    ebx,DWORD PTR [rax+rbp*2-0x5]"),
            (b"\x8b\x5c\x40\xfb",     "mov    ebx,DWORD PTR [rax+rax*2-0x5]"),
        ];
        expect_disasm(&code, expected);
        assert_eq!(expected_texts.len(), expected.len());
        for (et, &(_, t)) in expected_texts.iter().rev().zip(expected) {
            assert_eq!(et, t);
        }
    }

    #[test]
    fn sib_combinations_disp32() {
        let bases = [Reg::Ax, Reg::Sp, Reg::Bp, Reg::R12, Reg::R13, Reg::R15];
        let indexes = [Reg::Ax, Reg::Bp, Reg::R12, Reg::R13, Reg::R15];
        let mut code = Vec::<u8>::new();
        let mut expected_texts = Vec::new();
        for &base in &bases {
            for &index in &indexes {
                gen::mov32_r_rm(&mut code, Reg::Bx, Mem2::base(base).index_scale(index, 2).disp(-0x333));
                expected_texts.push(
                    format!("mov    ebx,DWORD PTR [{}+{}*2-0x333]", base.name64(), index.name64()));
            }
        }
        let expected: &[(&[u8], &str)] = &[
            (b"\x43\x8b\x9c\x7f\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r15+r15*2-0x333]"),
            (b"\x43\x8b\x9c\x6f\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r15+r13*2-0x333]"),
            (b"\x43\x8b\x9c\x67\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r15+r12*2-0x333]"),
            (b"\x41\x8b\x9c\x6f\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r15+rbp*2-0x333]"),
            (b"\x41\x8b\x9c\x47\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r15+rax*2-0x333]"),
            (b"\x43\x8b\x9c\x7d\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r13+r15*2-0x333]"),
            (b"\x43\x8b\x9c\x6d\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r13+r13*2-0x333]"),
            (b"\x43\x8b\x9c\x65\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r13+r12*2-0x333]"),
            (b"\x41\x8b\x9c\x6d\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r13+rbp*2-0x333]"),
            (b"\x41\x8b\x9c\x45\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r13+rax*2-0x333]"),
            (b"\x43\x8b\x9c\x7c\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r12+r15*2-0x333]"),
            (b"\x43\x8b\x9c\x6c\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r12+r13*2-0x333]"),
            (b"\x43\x8b\x9c\x64\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r12+r12*2-0x333]"),
            (b"\x41\x8b\x9c\x6c\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r12+rbp*2-0x333]"),
            (b"\x41\x8b\x9c\x44\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [r12+rax*2-0x333]"),
            (b"\x42\x8b\x9c\x7d\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [rbp+r15*2-0x333]"),
            (b"\x42\x8b\x9c\x6d\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [rbp+r13*2-0x333]"),
            (b"\x42\x8b\x9c\x65\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [rbp+r12*2-0x333]"),
            (b"\x8b\x9c\x6d\xcd\xfc\xff\xff",     "mov    ebx,DWORD PTR [rbp+rbp*2-0x333]"),
            (b"\x8b\x9c\x45\xcd\xfc\xff\xff",     "mov    ebx,DWORD PTR [rbp+rax*2-0x333]"),
            (b"\x42\x8b\x9c\x7c\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [rsp+r15*2-0x333]"),
            (b"\x42\x8b\x9c\x6c\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [rsp+r13*2-0x333]"),
            (b"\x42\x8b\x9c\x64\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [rsp+r12*2-0x333]"),
            (b"\x8b\x9c\x6c\xcd\xfc\xff\xff",     "mov    ebx,DWORD PTR [rsp+rbp*2-0x333]"),
            (b"\x8b\x9c\x44\xcd\xfc\xff\xff",     "mov    ebx,DWORD PTR [rsp+rax*2-0x333]"),
            (b"\x42\x8b\x9c\x78\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [rax+r15*2-0x333]"),
            (b"\x42\x8b\x9c\x68\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [rax+r13*2-0x333]"),
            (b"\x42\x8b\x9c\x60\xcd\xfc\xff\xff", "mov    ebx,DWORD PTR [rax+r12*2-0x333]"),
            (b"\x8b\x9c\x68\xcd\xfc\xff\xff",     "mov    ebx,DWORD PTR [rax+rbp*2-0x333]"),
            (b"\x8b\x9c\x40\xcd\xfc\xff\xff",     "mov    ebx,DWORD PTR [rax+rax*2-0x333]"),
        ];
        expect_disasm(&code, expected);
        assert_eq!(expected_texts.len(), expected.len());
        for (et, &(_, t)) in expected_texts.iter().rev().zip(expected) {
            assert_eq!(et, t);
        }
    }
}

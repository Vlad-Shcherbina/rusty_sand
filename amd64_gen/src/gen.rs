use super::*;

pub trait RegOrMem {
    fn is_reg(&self) -> bool;

    // return r, x, b bits of REX prefix
    fn encode_modrm(&self, r1: Reg, sink: &mut impl CodeSink) -> u8;
}

impl RegOrMem for Reg {
    fn is_reg(&self) -> bool { true }

    fn encode_modrm(&self, r1: Reg, sink: &mut impl CodeSink) -> u8 {
        let r1_modrm = (r1 as u8 & 7) << 3;
        let r1_rex = (r1 as u8 & 8) >> 1;
        let r2 = *self as u8;
        sink.prepend(&[0b11_000_000 | r1_modrm | (r2 & 7)]);
        r1_rex | r2 >> 3
    }
}

impl RegOrMem for RipRel {
    fn is_reg(&self) -> bool { false }

    fn encode_modrm(&self, r1: Reg, sink: &mut impl CodeSink) -> u8 {
        let r1_modrm = (r1 as u8 & 7) << 3;
        let r1_rex = (r1 as u8 & 8) >> 1;
        let disp = self.0;
        sink.prepend(&disp.to_le_bytes());
        sink.prepend(&[0b00_000_101 | r1_modrm]);
        r1_rex
    }
}

impl RegOrMem for Mem {
    fn is_reg(&self) -> bool { false }

    fn encode_modrm(&self, r1: Reg, sink: &mut impl CodeSink) -> u8 {
        let r1_modrm = (r1 as u8 & 7) << 3;
        let r1_rex = (r1 as u8 & 8) >> 1;
        let base = self.base as u8;
        #[allow(clippy::identity_op)]
        match i8::try_from(self.disp) {
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
                    sink.prepend(&self.disp.to_le_bytes());
                    sink.prepend(&[0b10_000_000 | r1_modrm | (base & 7)]);
                } else {
                    sink.prepend(&self.disp.to_le_bytes());
                    sink.prepend(&[0b10_000_100 | r1_modrm, 0b00_100_100]);
                }
        }
        r1_rex | base >> 3
    }
}

impl RegOrMem for MemSIB {
    fn is_reg(&self) -> bool { false }

    fn encode_modrm(&self, r1: Reg, sink: &mut impl CodeSink) -> u8 {
        let r1_modrm = (r1 as u8 & 7) << 3;
        let r1_rex = (r1 as u8 & 8) >> 1;
        assert!(self.index != Reg::Sp);
        let base = self.base as u8;
        let index = self.index as u8;
        let scale = match self.scale {
            1 => 0,
            2 => 1,
            4 => 2,
            8 => 3,
            _ => panic!("{}", self.scale),
        };
        let sib = scale << 6
                | (index & 7) << 3
                | base & 7;
        match i8::try_from(self.disp) {
            Ok(0) if base & 7 != 5 =>
                sink.prepend(&[0b00_000_100 | r1_modrm, sib]),
            Ok(disp) => {
                sink.prepend(&[0b01_000_100 | r1_modrm, sib, disp as u8]);
            }
            Err(_) => {
                sink.prepend(&self.disp.to_le_bytes());
                sink.prepend(&[0b10_000_100 | r1_modrm, sib]);
            }
        }
        r1_rex | base >> 3 | index >> 3 << 1
    }
}

impl<T: CodeSink> GenExt for T {}

pub trait GenExt: CodeSink + Sized {
    fn mov32_r_rm(&mut self, r: Reg, rm: impl RegOrMem) {
        let rex = rm.encode_modrm(r, self);
        self.prepend(&[0x8B]);
        if rex != 0 {
            self.prepend(&[rex | 0x40]);
        }
    }

    fn mov64_r_rm(&mut self, r: Reg, rm: impl RegOrMem) {
        let rex = rm.encode_modrm(r, self);
        self.prepend(&[rex | 0x48, 0x8B]);
    }

    fn mov32_rm_r(&mut self, rm: impl RegOrMem, r: Reg) {
        let rex = rm.encode_modrm(r, self);
        self.prepend(&[0x89]);
        if rex != 0 {
            self.prepend(&[rex | 0x40]);
        }
    }

    fn mov64_rm_r(&mut self, rm: impl RegOrMem, r: Reg) {
        let rex = rm.encode_modrm(r, self);
        self.prepend(&[rex | 0x48, 0x89]);
    }

    fn mov32_imm(&mut self, rm: impl RegOrMem, imm: i32) {
        self.prepend(&imm.to_le_bytes());
        let rex = rm.encode_modrm(0.try_into().unwrap(), self);
        self.prepend(&[0xc7]);
        if rex != 0 {
            self.prepend(&[rex | 0x40]);
        }
    }

    fn mov64_imm(&mut self, rm: impl RegOrMem, imm: i32) {
        self.prepend(&imm.to_le_bytes());
        let rex = rm.encode_modrm(0.try_into().unwrap(), self);
        self.prepend(&[rex | 0x48, 0xc7]);
    }

    fn movabs64_imm(&mut self, r: Reg, imm: i64) {
        let r = r as u8;
        self.prepend(&imm.to_le_bytes());
        self.prepend(&[0x48 | r >> 3, 0xb8 | r & 7]);
    }

    fn lea64(&mut self, r: Reg, m: impl RegOrMem) {
        assert!(!m.is_reg(), "lea with register");
        let rex = m.encode_modrm(r, self);
        self.prepend(&[rex | 0x48, 0x8d]);
    }

    fn binop32_r_rm(&mut self, op: Binop, r: Reg, rm: impl RegOrMem) {
        let rex = rm.encode_modrm(r, self);
        self.prepend(&[op as u8 * 8 + 3]);
        if rex != 0 {
            self.prepend(&[rex | 0x40]);
        }
    }

    fn binop64_r_rm(&mut self, op: Binop, r: Reg, rm: impl RegOrMem) {
        let rex = rm.encode_modrm(r, self);
        self.prepend(&[rex | 0x48, op as u8 * 8 + 3]);
    }

    fn binop32_rm_r(&mut self, op: Binop, rm: impl RegOrMem, r: Reg) {
        let rex = rm.encode_modrm(r, self);
        self.prepend(&[op as u8 * 8 + 1]);
        if rex != 0 {
            self.prepend(&[rex | 0x40]);
        }
    }

    fn binop64_rm_r(&mut self, op: Binop, rm: impl RegOrMem, r: Reg) {
        let rex = rm.encode_modrm(r, self);
        self.prepend(&[rex | 0x48, op as u8 * 8 + 1]);
    }

    fn binop32_imm(&mut self, op: Binop, rm: impl RegOrMem, imm: i32) {
        self.prepend(&imm.to_le_bytes());
        let rex = rm.encode_modrm((op as u8).try_into().unwrap(), self);
        self.prepend(&[0x81]);
        if rex != 0 {
            self.prepend(&[rex | 0x40]);
        }
    }

    fn binop64_imm(&mut self, op: Binop, rm: impl RegOrMem, imm: i32) {
        self.prepend(&imm.to_le_bytes());
        let rex = rm.encode_modrm((op as u8).try_into().unwrap(), self);
        self.prepend(&[rex | 0x48, 0x81]);
    }

    fn mul_op32(&mut self, op: MulOp, rm: impl RegOrMem) {
        let rex = rm.encode_modrm((op as u8).try_into().unwrap(), self);
        self.prepend(&[0xf7]);
        if rex != 0 {
            self.prepend(&[rex | 0x40]);
        }
    }

    fn mul_op64(&mut self, op: MulOp, rm: impl RegOrMem) {
        let rex = rm.encode_modrm((op as u8).try_into().unwrap(), self);
        self.prepend(&[rex | 0x48, 0xf7]);
    }

    fn push64(&mut self, rm: impl RegOrMem) {
        let rex = rm.encode_modrm(6.try_into().unwrap(), self);
        self.prepend(&[0xff]);
        if rex != 0 {
            // rex.W bit is implied for 64-bit (because 32-bit version is illegal),
            // no need to set it
            self.prepend(&[rex | 0x40]);
        }
    }

    fn pop64(&mut self, rm: impl RegOrMem) {
        let rex = rm.encode_modrm(0.try_into().unwrap(), self);
        self.prepend(&[0x8f]);
        if rex != 0 {
            // rex.W bit is implied for 64-bit (because 32-bit version is illegal),
            // no need to set it
            self.prepend(&[rex | 0x40]);
        }
    }

    fn jmp_cond(&mut self, cond: Cond, rel: impl Into<RelJumpTarget>) {
        let cond = cond as u8;
        match rel.into() {
            RelJumpTarget::Rel8(rel) => {
                self.prepend(&[0x70 | cond, rel as u8]);
            }
            RelJumpTarget::Rel32(rel) => {
                self.prepend(&rel.to_le_bytes());
                self.prepend(&[0x0f, 0x80 | cond]);
            }
        }
    }

    fn jmp_rel(&mut self, rel: impl Into<RelJumpTarget>) {
        match rel.into() {
            RelJumpTarget::Rel8(rel) => {
                self.prepend(&[0xeb, rel as u8]);
            }
            RelJumpTarget::Rel32(rel) => {
                self.prepend(&rel.to_le_bytes());
                self.prepend(&[0xe9]);
            }
        }
    }

    fn call_rel(&mut self, rel: i32) {
        self.prepend(&rel.to_le_bytes());
        self.prepend(&[0xe8]);
    }

    fn jmp_indirect(&mut self, target: impl RegOrMem) {
        let rex = target.encode_modrm(4.try_into().unwrap(), self);
        self.prepend(&[0xff]);
        if rex != 0 {
            self.prepend(&[rex | 0x40]);
        }
    }

    fn call_indirect(&mut self, target: impl RegOrMem) {
        let rex = target.encode_modrm(2.try_into().unwrap(), self);
        self.prepend(&[0xff]);
        if rex != 0 {
            self.prepend(&[rex | 0x40]);
        }
    }

    fn ret(&mut self) {
        self.prepend(&[0xc3]);
    }
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
            code.mov32_r_rm(r, r);
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
            code.mov64_r_rm(r, r);
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
                code.mov32_r_rm(r1, r2);
                code.mov32_rm_r(r1, r2);
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
                code.mov64_r_rm(r1, r2);
                code.mov64_rm_r(r1, r2);
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
        code.mov32_r_rm(Reg::Bx, Mem::base(Reg::Si));
        code.mov32_rm_r(Mem::base(Reg::R8), Reg::R11);
        expect_disasm(&code, &[
            (b"\x45\x89\x18", "mov    DWORD PTR [r8],r11d"),
            (b"\x8b\x1e",     "mov    ebx,DWORD PTR [rsi]"),
        ]);
    }

    #[test]
    fn mov_imm() {
        let mut code = Vec::<u8>::new();
        code.mov32_imm(Reg::R9, 0x42);
        code.mov32_imm(Mem::base(Reg::R11), 0x42);
        code.mov64_imm(Reg::Dx, -2);
        expect_disasm(&code, &[
            (b"\x48\xc7\xc2\xfe\xff\xff\xff", "mov    rdx,0xfffffffffffffffe"),
            (b"\x41\xc7\x03\x42\x00\x00\x00", "mov    DWORD PTR [r11],0x42"),
            (b"\x41\xc7\xc1\x42\x00\x00\x00", "mov    r9d,0x42"),
        ]);
    }

    #[test]
    fn movabs() {
        let mut code = Vec::<u8>::new();
        code.movabs64_imm(Reg::Bx, 0x1234567890);
        code.movabs64_imm(Reg::R9, 0x1234567890);
        expect_disasm(&code, &[
            (b"\x49\xb9\x90\x78\x56\x34\x12\x00\x00\x00", "movabs r9,0x1234567890"),
            (b"\x48\xbb\x90\x78\x56\x34\x12\x00\x00\x00", "movabs rbx,0x1234567890"),
        ]);
    }

    #[test]
    fn lea() {
        let mut code = Vec::<u8>::new();
        code.lea64(Reg::Cx, Mem::base(Reg::R11));
        code.lea64(Reg::Cx, RipRel(0x42));
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
            code.binop32_r_rm(op, Reg::Dx, Reg::Ax);
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
        code.binop32_r_rm(Binop::Or, Reg::Ax, Reg::Bx);
        code.binop32_rm_r(Binop::Or, Reg::Ax, Reg::Bx);
        code.binop64_r_rm(Binop::Xor, Reg::Ax, Reg::Bx);
        code.binop64_rm_r(Binop::Xor, Reg::Ax, Reg::Bx);
        code.binop32_imm(Binop::Sub, Reg::Cx, 0x42);
        code.binop64_imm(Binop::Sbb, Reg::Cx, 0x42);
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
            code.mul_op32(op, Reg::Bx);
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
        code.mul_op32(MulOp::Imul, Reg::R10);
        code.mul_op64(MulOp::Div, Reg::R10);
        expect_disasm(&code, &[
            (b"\x49\xf7\xf2", "div    r10"),
            (b"\x41\xf7\xea", "imul   r10d"),
        ]);
    }

    #[test]
    fn push_pop() {
        let mut code = Vec::<u8>::new();
        code.push64(Reg::Cx);
        code.push64(Mem::base(Reg::R15));
        code.pop64(Mem::base(Reg::R15));
        code.pop64(Reg::Cx);
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
                code.jmp_cond(cond, rel);
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
        code.jmp_rel(-2);
        code.jmp_rel(1000);
        code.jmp_rel(RelJumpTarget::Rel32(-5));
        code.call_rel(-5);
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
        code.jmp_indirect(Reg::Cx);
        code.call_indirect(Mem::base(Reg::R14));
        expect_disasm(&code, &[
            (b"\x41\xff\x16", "call   QWORD PTR [r14]"),
            (b"\xff\xe1",     "jmp    rcx"),
        ]);
    }

    #[test]
    fn ret() {
        let mut code = Vec::<u8>::new();
        code.ret();
        expect_disasm(&code, &[
            (b"\xc3", "ret"),
        ]);
    }

    #[test]
    fn rip_rel() {
        let mut code = Vec::<u8>::new();
        code.mov32_r_rm(Reg::Ax, RipRel(2));
        code.mov32_r_rm(Reg::Bx, RipRel(-2));
        expect_disasm(&code, &[
            (b"\x8b\x1d\xfe\xff\xff\xff", "mov    ebx,DWORD PTR [rip+0xfffffffffffffffe]"),
            (b"\x8b\x05\x02\x00\x00\x00", "mov    eax,DWORD PTR [rip+0x2]"),        ]);
    }

    #[test]
    fn base_only() {
        let mut code = Vec::<u8>::new();
        for base in Reg::all() {
            code.mov32_r_rm(Reg::Cx, Mem::base(base));
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
            code.mov32_r_rm(Reg::Cx, Mem::base(base).disp(5));
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
            code.mov32_r_rm(Reg::Cx, Mem::base(base).disp(0x333));
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
            code.mov32_r_rm(Reg::Cx, Mem::base(Reg::Bx).index_scale(Reg::Ax, scale));
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
                code.mov32_r_rm(Reg::Bx, Mem::base(base).index_scale(index, 2));
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
                code.mov32_r_rm(Reg::Bx, Mem::base(base).index_scale(index, 2).disp(-5));
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
                code.mov32_r_rm(Reg::Bx, Mem::base(base).index_scale(index, 2).disp(-0x333));
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

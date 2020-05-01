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
            if base & 7 != 4 {
                sink.prepend(&disp.to_le_bytes());
                sink.prepend(&[0b10_000_000 | r1_modrm | (base & 7)]);
            } else {
                sink.prepend(&disp.to_le_bytes());
                sink.prepend(&[0b10_000_100 | r1_modrm, 0b00_100_100]);
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
            sink.prepend(&disp.to_le_bytes());
            let sib = scale << 6
                    | (index & 7) << 3
                    | base & 7;
            sink.prepend(&[0b10_000_100 | r1_modrm, sib]);
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
            (b"\x45\x89\x98\x00\x00\x00\x00", "mov    DWORD PTR [r8+0x0],r11d"),
            (b"\x8b\x9e\x00\x00\x00\x00",     "mov    ebx,DWORD PTR [rsi+0x0]"),
        ]);
    }

    #[test]
    fn mov_imm() {
        let mut code = Vec::<u8>::new();
        gen::mov32_imm(&mut code, Reg::R9, 0x42);
        gen::mov32_imm(&mut code, Mem2::base(Reg::R11), 0x42);
        gen::mov64_imm(&mut code, Reg::Dx, -2);
        expect_disasm(&code, &[
            (b"\x48\xc7\xc2\xfe\xff\xff\xff",                 "mov    rdx,0xfffffffffffffffe"),
            (b"\x41\xc7\x83\x00\x00\x00\x00\x42\x00\x00\x00", "mov    DWORD PTR [r11+0x0],0x42"),
            (b"\x41\xc7\xc1\x42\x00\x00\x00",                 "mov    r9d,0x42"),
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
            (b"\x41\x8b\x8f\x00\x00\x00\x00",     "mov    ecx,DWORD PTR [r15+0x0]"),
            (b"\x41\x8b\x8e\x00\x00\x00\x00",     "mov    ecx,DWORD PTR [r14+0x0]"),
            (b"\x41\x8b\x8d\x00\x00\x00\x00",     "mov    ecx,DWORD PTR [r13+0x0]"),
            (b"\x41\x8b\x8c\x24\x00\x00\x00\x00", "mov    ecx,DWORD PTR [r12+0x0]"),
            (b"\x41\x8b\x8b\x00\x00\x00\x00",     "mov    ecx,DWORD PTR [r11+0x0]"),
            (b"\x41\x8b\x8a\x00\x00\x00\x00",     "mov    ecx,DWORD PTR [r10+0x0]"),
            (b"\x41\x8b\x89\x00\x00\x00\x00",     "mov    ecx,DWORD PTR [r9+0x0]"),
            (b"\x41\x8b\x88\x00\x00\x00\x00",     "mov    ecx,DWORD PTR [r8+0x0]"),
            (b"\x8b\x8f\x00\x00\x00\x00",         "mov    ecx,DWORD PTR [rdi+0x0]"),
            (b"\x8b\x8e\x00\x00\x00\x00",         "mov    ecx,DWORD PTR [rsi+0x0]"),
            (b"\x8b\x8d\x00\x00\x00\x00",         "mov    ecx,DWORD PTR [rbp+0x0]"),
            (b"\x8b\x8c\x24\x00\x00\x00\x00",     "mov    ecx,DWORD PTR [rsp+0x0]"),
            (b"\x8b\x8b\x00\x00\x00\x00",         "mov    ecx,DWORD PTR [rbx+0x0]"),
            (b"\x8b\x8a\x00\x00\x00\x00",         "mov    ecx,DWORD PTR [rdx+0x0]"),
            (b"\x8b\x89\x00\x00\x00\x00",         "mov    ecx,DWORD PTR [rcx+0x0]"),
            (b"\x8b\x88\x00\x00\x00\x00",         "mov    ecx,DWORD PTR [rax+0x0]"),
        ]);
    }

    #[test]
    fn sib_scales() {
        let mut code = Vec::<u8>::new();
        for &scale in &[1, 2, 4, 8] {
            gen::mov32_r_rm(&mut code, Reg::Cx, Mem2::base(Reg::Bx).index_scale(Reg::Ax, scale));
        }
        expect_disasm(&code, &[
            (b"\x8b\x8c\xc3\x00\x00\x00\x00", "mov    ecx,DWORD PTR [rbx+rax*8+0x0]"),
            (b"\x8b\x8c\x83\x00\x00\x00\x00", "mov    ecx,DWORD PTR [rbx+rax*4+0x0]"),
            (b"\x8b\x8c\x43\x00\x00\x00\x00", "mov    ecx,DWORD PTR [rbx+rax*2+0x0]"),
            (b"\x8b\x8c\x03\x00\x00\x00\x00", "mov    ecx,DWORD PTR [rbx+rax*1+0x0]"),
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
                    format!("mov    ebx,DWORD PTR [{}+{}*2+0x0]", base.name64(), index.name64()));
            }
        }
        let expected: &[(&[u8], &str)] = &[
            (b"\x43\x8b\x9c\x7f\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r15+r15*2+0x0]"),
            (b"\x43\x8b\x9c\x6f\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r15+r13*2+0x0]"),
            (b"\x43\x8b\x9c\x67\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r15+r12*2+0x0]"),
            (b"\x41\x8b\x9c\x6f\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r15+rbp*2+0x0]"),
            (b"\x41\x8b\x9c\x47\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r15+rax*2+0x0]"),
            (b"\x43\x8b\x9c\x7d\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r13+r15*2+0x0]"),
            (b"\x43\x8b\x9c\x6d\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r13+r13*2+0x0]"),
            (b"\x43\x8b\x9c\x65\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r13+r12*2+0x0]"),
            (b"\x41\x8b\x9c\x6d\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r13+rbp*2+0x0]"),
            (b"\x41\x8b\x9c\x45\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r13+rax*2+0x0]"),
            (b"\x43\x8b\x9c\x7c\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r12+r15*2+0x0]"),
            (b"\x43\x8b\x9c\x6c\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r12+r13*2+0x0]"),
            (b"\x43\x8b\x9c\x64\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r12+r12*2+0x0]"),
            (b"\x41\x8b\x9c\x6c\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r12+rbp*2+0x0]"),
            (b"\x41\x8b\x9c\x44\x00\x00\x00\x00", "mov    ebx,DWORD PTR [r12+rax*2+0x0]"),
            (b"\x42\x8b\x9c\x7d\x00\x00\x00\x00", "mov    ebx,DWORD PTR [rbp+r15*2+0x0]"),
            (b"\x42\x8b\x9c\x6d\x00\x00\x00\x00", "mov    ebx,DWORD PTR [rbp+r13*2+0x0]"),
            (b"\x42\x8b\x9c\x65\x00\x00\x00\x00", "mov    ebx,DWORD PTR [rbp+r12*2+0x0]"),
            (b"\x8b\x9c\x6d\x00\x00\x00\x00",     "mov    ebx,DWORD PTR [rbp+rbp*2+0x0]"),
            (b"\x8b\x9c\x45\x00\x00\x00\x00",     "mov    ebx,DWORD PTR [rbp+rax*2+0x0]"),
            (b"\x42\x8b\x9c\x7c\x00\x00\x00\x00", "mov    ebx,DWORD PTR [rsp+r15*2+0x0]"),
            (b"\x42\x8b\x9c\x6c\x00\x00\x00\x00", "mov    ebx,DWORD PTR [rsp+r13*2+0x0]"),
            (b"\x42\x8b\x9c\x64\x00\x00\x00\x00", "mov    ebx,DWORD PTR [rsp+r12*2+0x0]"),
            (b"\x8b\x9c\x6c\x00\x00\x00\x00",     "mov    ebx,DWORD PTR [rsp+rbp*2+0x0]"),
            (b"\x8b\x9c\x44\x00\x00\x00\x00",     "mov    ebx,DWORD PTR [rsp+rax*2+0x0]"),
            (b"\x42\x8b\x9c\x78\x00\x00\x00\x00", "mov    ebx,DWORD PTR [rax+r15*2+0x0]"),
            (b"\x42\x8b\x9c\x68\x00\x00\x00\x00", "mov    ebx,DWORD PTR [rax+r13*2+0x0]"),
            (b"\x42\x8b\x9c\x60\x00\x00\x00\x00", "mov    ebx,DWORD PTR [rax+r12*2+0x0]"),
            (b"\x8b\x9c\x68\x00\x00\x00\x00",     "mov    ebx,DWORD PTR [rax+rbp*2+0x0]"),
            (b"\x8b\x9c\x40\x00\x00\x00\x00",     "mov    ebx,DWORD PTR [rax+rax*2+0x0]"),
        ];
        expect_disasm(&code, expected);
        assert_eq!(expected_texts.len(), expected.len());
        for (et, &(_, t)) in expected_texts.iter().rev().zip(expected) {
            assert_eq!(et, t);
        }
    }
}

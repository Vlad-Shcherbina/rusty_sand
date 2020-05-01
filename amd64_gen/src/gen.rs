use super::*;

// return r, x, b bits of REX prefix
fn encode_modrm(sink: &mut impl CodeSink, r1: Reg, rm: RegOrMem2) -> u8 {
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
            sink.prepend(&disp.to_le_bytes());
            sink.prepend(&[0b10_000_000 | r1_modrm | (base & 7)]);
            r1_rex | base >> 3
        }
        _ => todo!()
    }
}

pub fn mov32_r_rm(sink: &mut impl CodeSink, r: Reg, rm: impl Into<RegOrMem2>) {
    let rm: RegOrMem2 = rm.into();
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[0x8B]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn mov64_r_rm(sink: &mut impl CodeSink, r: Reg, rm: impl Into<RegOrMem2>) {
    let rm: RegOrMem2 = rm.into();
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[rex | 0x48, 0x8B]);
}

pub fn mov32_rm_r(sink: &mut impl CodeSink, rm: impl Into<RegOrMem2>, r: Reg) {
    let rm: RegOrMem2 = rm.into();
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[0x89]);
    if rex != 0 {
        sink.prepend(&[rex | 0x40]);
    }
}

pub fn mov64_rm_r(sink: &mut impl CodeSink, rm: impl Into<RegOrMem2>, r: Reg) {
    let rm: RegOrMem2 = rm.into();
    let rex = encode_modrm(sink, r, rm);
    sink.prepend(&[rex | 0x48, 0x89]);
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
            if base as u8 & 7 == 4 {
                continue;  // TODO
            }
            gen::mov32_r_rm(&mut code, Reg::Ax, Mem2::base(base));
        }
        expect_disasm(&code, &[
            (b"\x41\x8b\x87\x00\x00\x00\x00", "mov    eax,DWORD PTR [r15+0x0]"),
            (b"\x41\x8b\x86\x00\x00\x00\x00", "mov    eax,DWORD PTR [r14+0x0]"),
            (b"\x41\x8b\x85\x00\x00\x00\x00", "mov    eax,DWORD PTR [r13+0x0]"),
            (b"\x41\x8b\x83\x00\x00\x00\x00", "mov    eax,DWORD PTR [r11+0x0]"),
            (b"\x41\x8b\x82\x00\x00\x00\x00", "mov    eax,DWORD PTR [r10+0x0]"),
            (b"\x41\x8b\x81\x00\x00\x00\x00", "mov    eax,DWORD PTR [r9+0x0]"),
            (b"\x41\x8b\x80\x00\x00\x00\x00", "mov    eax,DWORD PTR [r8+0x0]"),
            (b"\x8b\x87\x00\x00\x00\x00",     "mov    eax,DWORD PTR [rdi+0x0]"),
            (b"\x8b\x86\x00\x00\x00\x00",     "mov    eax,DWORD PTR [rsi+0x0]"),
            (b"\x8b\x85\x00\x00\x00\x00",     "mov    eax,DWORD PTR [rbp+0x0]"),
            (b"\x8b\x83\x00\x00\x00\x00",     "mov    eax,DWORD PTR [rbx+0x0]"),
            (b"\x8b\x82\x00\x00\x00\x00",     "mov    eax,DWORD PTR [rdx+0x0]"),
            (b"\x8b\x81\x00\x00\x00\x00",     "mov    eax,DWORD PTR [rcx+0x0]"),
            (b"\x8b\x80\x00\x00\x00\x00",     "mov    eax,DWORD PTR [rax+0x0]"),
        ]);
    }
}

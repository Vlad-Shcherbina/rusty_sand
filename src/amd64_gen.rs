use std::convert::{TryFrom, TryInto};

#[derive(Clone, Copy, Debug)]
pub enum R8 {
    Al = 0,
    Cl,
    Dl,
    Bl,
    // TODO:
    //   ah, ch, dh, bh  -- not addressable in REX prefix insn forms
    //   spl, bpl, sil, dil  -- only addressable in REX prefix insn forms
    R8b = 8,
    R9b,
    R10b,
    R11b,
    R12b,
    R13b,
    R14b,
    R15b,
}

impl R8 {
    pub fn all() -> impl Iterator<Item=R8> {
        (0..4).chain(8..16).map(|x| x.try_into().unwrap())
    }
}

impl std::fmt::Display for R8 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut name = format!("{:?}", self);
        name.make_ascii_lowercase();
        write!(f, "{}", name)
    }
}

impl TryFrom<u8> for R8 {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => R8::Al,
            1 => R8::Bl,
            2 => R8::Cl,
            3 => R8::Dl,
            8 => R8::R8b,
            9 => R8::R9b,
            10 => R8::R10b,
            11 => R8::R11b,
            12 => R8::R12b,
            13 => R8::R13b,
            14 => R8::R14b,
            15 => R8::R15b,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub enum R16 {
    Ax = 0,
    Cx,
    Dx,
    Bx,
    Sp,
    Bp,
    Si,
    Di,
    R8w,
    R9w,
    R10w,
    R11w,
    R12w,
    R13w,
    R14w,
    R15w,
}

impl R16 {
    pub fn all() -> impl Iterator<Item=R16> {
        (0..16).map(|x| x.try_into().unwrap())
    }
}

impl std::fmt::Display for R16 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut name = format!("{:?}", self);
        name.make_ascii_lowercase();
        write!(f, "{}", name)
    }
}

impl TryFrom<u8> for R16 {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => R16::Ax,
            1 => R16::Cx,
            2 => R16::Dx,
            3 => R16::Bx,
            4 => R16::Sp,
            5 => R16::Bp,
            6 => R16::Si,
            7 => R16::Di,
            8 => R16::R8w,
            9 => R16::R9w,
            10 => R16::R10w,
            11 => R16::R11w,
            12 => R16::R12w,
            13 => R16::R13w,
            14 => R16::R14w,
            15 => R16::R15w,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub enum R32 {
    Eax = 0,
    Ecx,
    Edx,
    Ebx,
    Esp,
    Ebp,
    Esi,
    Edi,
    R8d,
    R9d,
    R10d,
    R11d,
    R12d,
    R13d,
    R14d,
    R15d,
}

impl R32 {
    pub fn all() -> impl Iterator<Item=R32> {
        (0..16).map(|x| x.try_into().unwrap())
    }
}

impl std::fmt::Display for R32 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut name = format!("{:?}", self);
        name.make_ascii_lowercase();
        write!(f, "{}", name)
    }
}

impl TryFrom<u8> for R32 {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => R32::Eax,
            1 => R32::Ecx,
            2 => R32::Edx,
            3 => R32::Ebx,
            4 => R32::Esp,
            5 => R32::Ebp,
            6 => R32::Esi,
            7 => R32::Edi,
            8 => R32::R8d,
            9 => R32::R9d,
            10 => R32::R10d,
            11 => R32::R11d,
            12 => R32::R12d,
            13 => R32::R13d,
            14 => R32::R14d,
            15 => R32::R15d,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum R64 {
    Rax = 0,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl R64 {
    pub fn all() -> impl Iterator<Item=R64> {
        (0..16).map(|x| x.try_into().unwrap())
    }
}

impl std::fmt::Display for R64 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut name = format!("{:?}", self);
        name.make_ascii_lowercase();
        write!(f, "{}", name)
    }
}

impl TryFrom<u8> for R64 {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => R64::Rax,
            1 => R64::Rcx,
            2 => R64::Rdx,
            3 => R64::Rbx,
            4 => R64::Rsp,
            5 => R64::Rbp,
            6 => R64::Rsi,
            7 => R64::Rdi,
            8 => R64::R8,
            9 => R64::R9,
            10 => R64::R10,
            11 => R64::R11,
            12 => R64::R12,
            13 => R64::R13,
            14 => R64::R14,
            15 => R64::R15,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Mem {
    base: R64,
    index_scale: Option<(R64, u8)>,
    disp: i32,
}

impl Mem {
    pub fn new(base: R64) -> Mem {
        Mem {
            base,
            index_scale: None,
            disp: 0,
        }
    }

    pub fn index_scale(self, index: R64, scale: u8) -> Mem {
        assert!(self.index_scale.is_none());
        assert!(scale == 1 || scale == 2 || scale == 4 || scale == 8);
        Mem {
            index_scale: Some((index, scale)),
            ..self
        }
    }

    pub fn disp(self, disp: i32) -> Mem {
        assert_eq!(self.disp, 0);
        Mem {
            disp,
            ..self
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Operand {
    R8(R8),
    R16(R16),
    R32(R32),
    R64(R64),
    RipRel(i32),
    Mem(Mem),
    Imm8(i8),
    Imm16(i16),
    Imm32(i32),
    Imm64(i64),
}

impl Operand {
    fn size_bits(&self) -> Option<u8> {
        match self {
            Operand::R8(_) => Some(8),
            Operand::R16(_) => Some(16),
            Operand::R32(_) => Some(32),
            Operand::R64(_) => Some(64),
            Operand::RipRel(_) => None,
            Operand::Mem(_) => None,
            Operand::Imm8(_) => Some(8),
            Operand::Imm16(_) => Some(16),
            Operand::Imm32(_) => Some(32),
            Operand::Imm64(_) => Some(64),
        }
    }
}

impl From<R8> for Operand {
    fn from(r: R8) -> Self { Self::R8(r) }
}

impl From<R16> for Operand {
    fn from(r: R16) -> Self { Self::R16(r) }
}

impl From<R32> for Operand {
    fn from(r: R32) -> Self { Self::R32(r) }
}

impl From<R64> for Operand {
    fn from(r: R64) -> Self { Self::R64(r) }
}

impl From<Mem> for Operand {
    fn from(m: Mem) -> Self { Self::Mem(m) }
}

impl From<i8> for Operand {
    fn from(imm: i8) -> Self { Self::Imm8(imm) }
}

impl From<i16> for Operand {
    fn from(imm: i16) -> Self { Self::Imm16(imm) }
}

impl From<i32> for Operand {
    fn from(imm: i32) -> Self { Self::Imm32(imm) }
}

impl From<i64> for Operand {
    fn from(imm: i64) -> Self { Self::Imm64(imm) }
}

#[derive(Default)]
struct RmEncoding {
    rex_rxb: u8,
    modrm: u8,
    buf: [u8; 5],  // optional sib, disp
    buf_len: usize,
}

impl RmEncoding {
    fn from_reg(r: u8) -> Self {
        Self {
            rex_rxb: r as u8 >> 3,
            modrm: 0b11_000_000 | (r as u8 & 7),
            buf: [0; 5],
            buf_len: 0,
        }
    }

    fn from_reg_or_mem(op: Operand) -> Self {
        match op {
            Operand::R8(r) => Self::from_reg(r as u8),
            Operand::R16(r) => Self::from_reg(r as u8),
            Operand::R32(r) => Self::from_reg(r as u8),
            Operand::R64(r) => Self::from_reg(r as u8),
            Operand::RipRel(disp) => {
                let mut buf = [0; 5];
                buf[..4].copy_from_slice(&disp.to_le_bytes());
                Self {
                    rex_rxb: 0,
                    modrm: 0b00_000_101,
                    buf,
                    buf_len: 4,
                }
            }
            Operand::Mem(mem) => {
                match mem.index_scale {
                    None => if mem.base as u8 & 7 != 4 {
                        let mut buf = [0; 5];
                        // TODO: use disp8 and no disp forms when appropriate
                        buf[..4].copy_from_slice(&mem.disp.to_le_bytes());
                        Self {
                            rex_rxb: mem.base as u8 >> 3,
                            modrm: 0b10_000_000 | (mem.base as u8 & 7),
                            buf,
                            buf_len: 4,
                        }
                    } else {
                        let sib = 0b00_100_100;
                        let mut buf = [sib, 0, 0, 0, 0];
                        buf[1..].copy_from_slice(&mem.disp.to_le_bytes());
                        // TODO: use disp8 and no disp forms when appropriate
                        Self {
                            rex_rxb: mem.base as u8 >> 3,
                            modrm: 0b10_000_100,
                            buf,
                            buf_len: 5,
                        }
                    }
                    Some((index, scale)) => {
                        assert!(index != R64::Rsp);
                        let scale = match scale {
                            1 => 0,
                            2 => 1,
                            4 => 2,
                            8 => 3,
                            _ => panic!("{}", scale),
                        };
                        let sib = (scale << 6)
                            | ((index as u8 & 7) << 3)
                            | (mem.base as u8 & 7);
                        let mut buf = [sib, 0, 0, 0, 0];
                        // TODO: use disp8 and no disp forms when appropriate
                        buf[1..].copy_from_slice(&mem.disp.to_le_bytes());
                        Self {
                            rex_rxb: (mem.base as u8 >> 3) | (index as u8 >> 3 << 1),
                            modrm: 0b10_000_100,
                            buf,
                            buf_len: 5,
                        }
                    }
                }
            }
            _ => panic!("{:?}", op),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Binop {
    Add = 0,
    Or,
    Adc,
    Sbb,
    And,
    Sub,
    Xor,
    Cmp,
}

#[derive(Default, Debug)]
pub struct Gen {
    buf: [u8; 15],
    buf_len: usize,
}

impl Gen {
    fn write_u8(&mut self, x: u8) {
        self.buf[self.buf_len] = x;
        self.buf_len += 1;
    }

    fn write_slice(&mut self, xs: &[u8]) {
        self.buf[self.buf_len..][..xs.len()].copy_from_slice(xs);
        self.buf_len += xs.len();
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.buf[0..self.buf_len]
    }

    pub fn binop(op: Binop, dst: impl Into<Operand>, src: impl Into<Operand>) -> Gen {
        let src: Operand = src.into();
        let dst: Operand = dst.into();

        let size_bits = match (src.size_bits(), dst.size_bits()) {
            (Some(s1), Some(s2)) => {
                assert_eq!(s1, s2);
                s1
            },
            (Some(s1), None) => s1,
            (None, Some(s2)) => s2,
            (None, None) => panic!("{:?} {:?}", src, dst),
        };

        let mut gen = Gen::default();
        if size_bits == 16 {
            gen.write_u8(0x66);
        }

        let imm8;
        let imm16;
        let imm32;
        let imm_slice;

        let reg;
        let rm;
        let opcode;
        match (dst, src) {
            (dst, Operand::Imm8(imm)) => {
                rm = dst;
                reg = op as u8;
                opcode = 0x80;
                imm8 = imm as u8;
                imm_slice = std::slice::from_ref(&imm8);
            }
            (dst, Operand::Imm16(imm)) => {
                rm = dst;
                reg = op as u8;
                opcode = 0x81;
                imm16 = (imm as u16).to_le_bytes();
                imm_slice = &imm16;
            }
            (dst, Operand::Imm32(imm)) => {
                rm = dst;
                reg = op as u8;
                opcode = 0x81;
                imm32 = (imm as u32).to_le_bytes();
                imm_slice = &imm32;
            }
            (dst, Operand::Imm64(imm)) => {
                rm = dst;
                reg = op as u8;
                opcode = 0x81;
                imm32 = (i32::try_from(imm).unwrap() as u32).to_le_bytes();
                imm_slice = &imm32;
            }
            (_, Operand::Mem(_)) |
            (_, Operand::RipRel(_)) => {
                reg = match dst {
                    Operand::R8(dst) => dst as u8,
                    Operand::R16(dst) => dst as u8,
                    Operand::R32(dst) => dst as u8,
                    Operand::R64(dst) => dst as u8,
                    _ => panic!("{:?}", dst)
                };
                rm = src;
                opcode = match size_bits {
                    8 => op as u8 * 8 + 2,
                    16 | 32 | 64 => op as u8 * 8 + 3,
                    _ => unreachable!(),
                };
                imm_slice = &[];
            }
            _ => {
                reg = match src {
                    Operand::R8(src) => src as u8,
                    Operand::R16(src) => src as u8,
                    Operand::R32(src) => src as u8,
                    Operand::R64(src) => src as u8,
                    _ => panic!("{:?}", src)
                };
                rm = dst;
                opcode = match size_bits {
                    8 => op as u8 * 8,
                    16 | 32 | 64 => op as u8 * 8 + 1,
                    _ => unreachable!(),
                };
                imm_slice = &[];
            }
        }

        let enc = RmEncoding::from_reg_or_mem(rm);
        let mut rex = enc.rex_rxb | ((reg & 8) >> 1);
        if size_bits == 64 {
            rex |= 8;
        }
        if rex != 0 {
            gen.write_u8(0x40 | rex);
        }

        gen.write_u8(opcode);

        gen.write_u8(enc.modrm | ((reg & 7) << 3));

        gen.write_slice(&enc.buf[..enc.buf_len]);
        gen.write_slice(imm_slice);

        gen
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binutils::Obj;

    #[test]
    fn add_r8_r8() {
        for r1 in R8::all() {
            let mut bytes = Vec::<u8>::new();
            let mut expected = Vec::new();
            for r2 in R8::all() {
                bytes.extend_from_slice(Gen::binop(Binop::Add, r1, r2).as_slice());
                expected.push(format!("add    %{},%{}", r2, r1));
            }
            let insns = Obj::from_bytes(&bytes).insns();
            assert_eq!(insns.len(), expected.len());
            for (insn, expected) in insns.iter().zip(expected) {
                assert_eq!(insn.text, expected);
            }
        }
    }

    #[test]
    fn add_r16_r16() {
        for r1 in R16::all() {
            let mut bytes = Vec::<u8>::new();
            let mut expected = Vec::new();
            for r2 in R16::all() {
                bytes.extend_from_slice(Gen::binop(Binop::Add, r1, r2).as_slice());
                expected.push(format!("add    %{},%{}", r2, r1));
            }
            let insns = Obj::from_bytes(&bytes).insns();
            assert_eq!(insns.len(), expected.len());
            for (insn, expected) in insns.iter().zip(expected) {
                assert_eq!(insn.text, expected);
            }
        }
    }

    #[test]
    fn add_r32_r32() {
        for r1 in R32::all() {
            let mut bytes = Vec::<u8>::new();
            let mut expected = Vec::new();
            for r2 in R32::all() {
                bytes.extend_from_slice(Gen::binop(Binop::Add, r1, r2).as_slice());
                expected.push(format!("add    %{},%{}", r2, r1));
            }
            let insns = Obj::from_bytes(&bytes).insns();
            assert_eq!(insns.len(), expected.len());
            for (insn, expected) in insns.iter().zip(expected) {
                assert_eq!(insn.text, expected);
            }
        }
    }

    #[test]
    fn add_r64_r64() {
        for r1 in R64::all() {
            let mut bytes = Vec::<u8>::new();
            let mut expected = Vec::new();
            for r2 in R64::all() {
                bytes.extend_from_slice(Gen::binop(Binop::Add, r1, r2).as_slice());
                expected.push(format!("add    %{},%{}", r2, r1));
            }
            let insns = Obj::from_bytes(&bytes).insns();
            assert_eq!(insns.len(), expected.len());
            for (insn, expected) in insns.iter().zip(expected) {
                assert_eq!(insn.text, expected);
            }
        }
    }

    #[test]
    fn all_binops() {
        use super::Binop::*;
        let mut bytes = Vec::<u8>::new();
        let mut expected = Vec::new();
        for &binop in &[Add, Or, Adc, Sbb, And, Sub, Xor, Cmp] {
            let mut binop_name = format!("{:?}", binop);
            binop_name.make_ascii_lowercase();

            bytes.extend_from_slice(Gen::binop(binop, R32::Ebx, R32::Ecx).as_slice());
            expected.push(format!("{:3}    %ecx,%ebx", binop_name));
        }
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), expected.len());
        for (insn, expected) in insns.iter().zip(expected) {
            assert_eq!(insn.text, expected);
        }
    }

    #[test]
    fn binop_imm() {
        let insns = Obj::from_bytes(Gen::binop(Binop::Adc, R8::Bl, 0x42i8).as_slice()).insns();
        assert_eq!(insns.len(), 1);
        assert_eq!(insns[0].text, "adc    $0x42,%bl");

        let insns = Obj::from_bytes(Gen::binop(Binop::Adc, R16::Bx, 0x42i16).as_slice()).insns();
        assert_eq!(insns.len(), 1);
        assert_eq!(insns[0].text, "adc    $0x42,%bx");

        let insns = Obj::from_bytes(Gen::binop(Binop::Adc, R32::Ebx, 0x42i32).as_slice()).insns();
        assert_eq!(insns.len(), 1);
        assert_eq!(insns[0].text, "adc    $0x42,%ebx");

        let insns = Obj::from_bytes(Gen::binop(Binop::Adc, R64::Rbx, 0x42i64).as_slice()).insns();
        assert_eq!(insns.len(), 1);
        assert_eq!(insns[0].text, "adc    $0x42,%rbx");
    }

    #[test]
    fn binop_rip_rel() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend_from_slice(
            Gen::binop(Binop::Adc, Operand::RipRel(0x42), R8::Dl).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Adc, Operand::RipRel(0x42), R16::Dx).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Adc, Operand::RipRel(0x42), R32::Edx).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Adc, Operand::RipRel(0x42), R64::Rdx).as_slice());
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), 4);
        assert_eq!(insns[0].text, "adc    %dl,0x42(%rip)");
        assert_eq!(insns[1].text, "adc    %dx,0x42(%rip)");
        assert_eq!(insns[2].text, "adc    %edx,0x42(%rip)");
        assert_eq!(insns[3].text, "adc    %rdx,0x42(%rip)");
    }

    #[test]
    fn binop_directions() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend_from_slice(
            Gen::binop(Binop::Add, Mem::new(R64::Rcx), R8::Dl).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Add, R8::Dl, Mem::new(R64::Rcx)).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Add, Mem::new(R64::Rcx), R32::Edx).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Add, R32::Edx, Mem::new(R64::Rcx)).as_slice());
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), 4);
        assert_eq!(insns[0].text, "add    %dl,0x0(%rcx)");
        assert_eq!(insns[1].text, "add    0x0(%rcx),%dl");
        assert_eq!(insns[2].text, "add    %edx,0x0(%rcx)");
        assert_eq!(insns[3].text, "add    0x0(%rcx),%edx");
    }

    #[test]
    fn mem_single_reg() {
        let mut bytes = Vec::<u8>::new();
        let mut expected = Vec::new();
        for r in R64::all() {
            bytes.extend_from_slice(Gen::binop(Binop::Add, Mem::new(r), R32::Eax).as_slice());
            expected.push(format!("add    %eax,0x0(%{})", r));
        }
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), expected.len());
        for (insn, expected) in insns.iter().zip(expected) {
            assert_eq!(insn.text, expected);
        }
    }

    #[test]
    fn sib() {
        for base in R64::all() {
            let mut bytes = Vec::<u8>::new();
            let mut expected = Vec::new();
            for index in R64::all() {
                if index == R64::Rsp {
                    continue;
                }
                bytes.extend_from_slice(
                    Gen::binop(Binop::Add, Mem::new(base).index_scale(index, 4).disp(0x42), R32::Eax).as_slice());
                expected.push(format!("add    %eax,0x42(%{},%{},4)", base, index));
            }
            let insns = Obj::from_bytes(&bytes).insns();
            assert_eq!(insns.len(), expected.len());
            for (insn, expected) in insns.iter().zip(expected) {
                assert_eq!(insn.text, expected);
            }
        }
    }
}

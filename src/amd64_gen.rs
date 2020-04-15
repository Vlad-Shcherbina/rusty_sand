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

#[derive(Clone, Copy, Debug)]
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
    ImmU32(u32),
    // TODO: imm u8, imm i8, etc.
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
            Operand::ImmU32(_) => Some(32),
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

impl From<u32> for Operand {
    fn from(imm: u32) -> Self { Self::ImmU32(imm) }
}

#[derive(Default)]
struct RmEncoding {
    rex_rxb: u8,
    modrm: u8,
    buf: [u8; 9],  // optional sib, disp, imm
    buf_len: usize,
}

impl RmEncoding {
    fn from_reg(r: u8) -> Self {
        Self {
            rex_rxb: r as u8 >> 3,
            modrm: 0b11_000_000 | (r as u8 & 7),
            buf: [0; 9],
            buf_len: 0,
        }
    }

    fn from_reg_or_mem(op: Operand) -> Self {
        match op {
            Operand::R8(r) => Self::from_reg(r as u8),
            Operand::R16(r) => Self::from_reg(r as u8),
            Operand::R32(r) => Self::from_reg(r as u8),
            Operand::R64(r) => Self::from_reg(r as u8),
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

    pub fn as_slice(&self) -> &[u8] {
        &self.buf[0..self.buf_len]
    }

    pub fn binop(op: Binop, src: impl Into<Operand>, dst: impl Into<Operand>) -> Gen {
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
        let src = match src {
            Operand::R8(src) => src as u8,
            Operand::R16(src) => src as u8,
            Operand::R32(src) => src as u8,
            Operand::R64(src) => src as u8,
            _ => panic!("{:?}", src)
        };

        if size_bits == 16 {
            gen.write_u8(0x66);
        }

        let enc = RmEncoding::from_reg_or_mem(dst);
        let mut rex = enc.rex_rxb | ((src & 8) >> 1);
        if size_bits == 64 {
            rex |= 8;
        }
        if rex != 0 {
            gen.write_u8(0x40 | rex);
        }

        // opcode
        gen.write_u8(match size_bits {
            8 => op as u8 * 8,
            16 | 32 | 64 => op as u8 * 8 + 1,
            _ => unreachable!(),
        });

        gen.write_u8(enc.modrm | ((src & 7) << 3));

        gen.buf[gen.buf_len .. gen.buf_len + enc.buf_len].copy_from_slice(
            &enc.buf[..enc.buf_len]);

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
                expected.push(format!("add    %{},%{}", r1, r2));
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
                expected.push(format!("add    %{},%{}", r1, r2));
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
                expected.push(format!("add    %{},%{}", r1, r2));
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
                expected.push(format!("add    %{},%{}", r1, r2));
            }
            let insns = Obj::from_bytes(&bytes).insns();
            assert_eq!(insns.len(), expected.len());
            for (insn, expected) in insns.iter().zip(expected) {
                assert_eq!(insn.text, expected);
            }
        }
    }

    #[test]
    fn binops() {
        use super::Binop::*;
        let mut bytes = Vec::<u8>::new();
        let mut expected = Vec::new();
        for &binop in &[Add, Or, Adc, Sbb, And, Sub, Xor, Cmp] {
            let mut binop_name = format!("{:?}", binop);
            binop_name.make_ascii_lowercase();

            bytes.extend_from_slice(Gen::binop(binop, R32::Ebx, R32::Ecx).as_slice());
            expected.push(format!("{:3}    %ebx,%ecx", binop_name));
        }
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), expected.len());
        for (insn, expected) in insns.iter().zip(expected) {
            assert_eq!(insn.text, expected);
        }
    }
}

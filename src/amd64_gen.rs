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
pub enum Addr {
    RipRel(i32),
    Sib {
        base: R64,
        index_scale: Option<(R64, u8)>,
        disp: i32,
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Mem {
    addr: Addr,
    size: Option<u8>,
}

impl Mem {
    pub fn rip_rel(disp: i32) -> Mem {
        Mem {
            addr: Addr::RipRel(disp),
            size: None
        }
    }

    pub fn base(base: R64) -> Mem {
        Mem {
            addr: Addr::Sib {
                base,
                index_scale: None,
                disp: 0,
            },
            size: None,
        }
    }

    pub fn index_scale(self, index: R64, scale: u8) -> Mem {
        assert!(scale == 1 || scale == 2 || scale == 4 || scale == 8);
        match self.addr {
            Addr::RipRel(_) => panic!(),
            Addr::Sib { base, index_scale, disp } => {
                assert!(index_scale.is_none());
                Mem {
                    addr: Addr::Sib {
                        base,
                        index_scale: Some((index, scale)),
                        disp
                    },
                    size: self.size,
                }
            }
        }
    }

    pub fn disp(self, new_disp: i32) -> Mem {
        match self.addr {
            Addr::RipRel(_) => panic!(),
            Addr::Sib { base, index_scale, disp } => {
                assert_eq!(disp, 0);
                Mem {
                    addr: Addr::Sib {
                        base,
                        index_scale,
                        disp: new_disp,
                    },
                    size: self.size,
                }
            }
        }
    }

    pub fn size(self, size: u8) -> Mem {
        assert!(self.size.is_none());
        Mem {
            addr: self.addr,
            size: Some(size),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Operand {
    R8(R8),
    R16(R16),
    R32(R32),
    R64(R64),
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
            Operand::Mem(Mem { size, .. }) => *size,
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
            rex_rxb: r >> 3,
            modrm: 0b11_000_000 | (r & 7),
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
            Operand::Mem(mem) => Self::from_mem(mem),
            _ => panic!("{:?}", op),
        }
    }

    fn from_mem(mem: Mem) -> Self {
        match mem.addr {
            Addr::RipRel(disp) => {
                let mut buf = [0; 5];
                buf[..4].copy_from_slice(&disp.to_le_bytes());
                Self {
                    rex_rxb: 0,
                    modrm: 0b00_000_101,
                    buf,
                    buf_len: 4,
                }
            }
            Addr::Sib { base, index_scale, disp } => {
                match index_scale {
                    None => if base as u8 & 7 != 4 {
                        let mut buf = [0; 5];
                        // TODO: use disp8 and no disp forms when appropriate
                        buf[..4].copy_from_slice(&disp.to_le_bytes());
                        Self {
                            rex_rxb: base as u8 >> 3,
                            modrm: 0b10_000_000 | (base as u8 & 7),
                            buf,
                            buf_len: 4,
                        }
                    } else {
                        let sib = 0b00_100_100;
                        let mut buf = [sib, 0, 0, 0, 0];
                        buf[1..].copy_from_slice(&disp.to_le_bytes());
                        // TODO: use disp8 and no disp forms when appropriate
                        Self {
                            rex_rxb: base as u8 >> 3,
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
                            | (base as u8 & 7);
                        let mut buf = [sib, 0, 0, 0, 0];
                        // TODO: use disp8 and no disp forms when appropriate
                        buf[1..].copy_from_slice(&disp.to_le_bytes());
                        Self {
                            rex_rxb: (base as u8 >> 3) | (index as u8 >> 3 << 1),
                            modrm: 0b10_000_100,
                            buf,
                            buf_len: 5,
                        }
                    }
                }
            }
        }
    }
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

impl Gen {
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
            (_, Operand::Mem(_)) => {
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

impl Gen {
    pub fn mov(dst: impl Into<Operand>, src: impl Into<Operand>) -> Gen {
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
        let enc;
        let opcode;
        match (dst, src) {
            // TODO: special encoding for
            //   mov r8, imm8    (shorter)
            //   mov r16, imm16  (shorter)
            //   mov r32, imm32  (shorter)
            //   mov r64, imm64  (actually allows imm64)
            (dst, Operand::Imm8(imm)) => {
                enc = RmEncoding::from_reg_or_mem(dst);
                reg = 0;
                opcode = 0xc6;
                imm8 = imm as u8;
                imm_slice = std::slice::from_ref(&imm8);
            }
            (dst, Operand::Imm16(imm)) => {
                enc = RmEncoding::from_reg_or_mem(dst);
                reg = 0;
                opcode = 0xc7;
                imm16 = (imm as u16).to_le_bytes();
                imm_slice = &imm16;
            }
            (dst, Operand::Imm32(imm)) => {
                enc = RmEncoding::from_reg_or_mem(dst);
                reg = 0;
                opcode = 0xc7;
                imm32 = (imm as u32).to_le_bytes();
                imm_slice = &imm32;
            }
            (dst, Operand::Imm64(imm)) => {
                enc = RmEncoding::from_reg_or_mem(dst);
                reg = 0;
                opcode = 0xc7;
                imm32 = (i32::try_from(imm).unwrap() as u32).to_le_bytes();
                imm_slice = &imm32;
            }
            (_, Operand::Mem(src)) => {
                reg = match dst {
                    Operand::R8(dst) => dst as u8,
                    Operand::R16(dst) => dst as u8,
                    Operand::R32(dst) => dst as u8,
                    Operand::R64(dst) => dst as u8,
                    _ => panic!("{:?}", dst)
                };
                enc = RmEncoding::from_mem(src);
                opcode = match size_bits {
                    8 => 0x8a,
                    16 | 32 | 64 => 0x8b,
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
                enc = RmEncoding::from_reg_or_mem(dst);
                opcode = match size_bits {
                    8 => 0x88,
                    16 | 32 | 64 => 0x89,
                    _ => unreachable!(),
                };
                imm_slice = &[];
            }
        }
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

impl Gen {
    pub fn push(op: impl Into<Operand>) -> Gen {
        let op: Operand = op.into();
        let mut gen = Gen::default();
        let size_bits = op.size_bits().unwrap();
        assert!(size_bits == 16 || size_bits == 64);
        if size_bits == 16 {
            gen.write_u8(0x66);
        }
        let enc = match op {
            // TODO: short forms for 'push r16', 'push r64'
            Operand::R16(r) => RmEncoding::from_reg(r as u8),
            Operand::R32(_) => panic!(),
            Operand::R64(r) => RmEncoding::from_reg(r as u8),
            Operand::Mem(m) => RmEncoding::from_mem(m),
            Operand::Imm16(_) => todo!(),
            Operand::Imm64(_) => todo!(),
            _ => panic!(),
        };
        let rex = enc.rex_rxb;
        // rex.W bit is implied for 64-bit (because 32-bit version is illegal),
        // no need to set it
        if rex != 0 {
            gen.write_u8(0x40 | rex);
        }
        gen.write_u8(0xff);
        gen.write_u8(enc.modrm | (6 << 3));
        gen.write_slice(&enc.buf[..enc.buf_len]);
        gen
    }

    pub fn pop(op: impl Into<Operand>) -> Gen {
        let op: Operand = op.into();
        let mut gen = Gen::default();
        let size_bits = op.size_bits().unwrap();
        assert!(size_bits == 16 || size_bits == 64);
        if size_bits == 16 {
            gen.write_u8(0x66);
        }
        let enc = match op {
            // TODO: short forms for 'pop r16', 'pop r64'
            Operand::R16(r) => RmEncoding::from_reg(r as u8),
            Operand::R32(_) => panic!(),
            Operand::R64(r) => RmEncoding::from_reg(r as u8),
            Operand::Mem(m) => RmEncoding::from_mem(m),
            Operand::Imm16(_) => todo!(),
            Operand::Imm64(_) => todo!(),
            _ => panic!(),
        };
        let rex = enc.rex_rxb;
        // rex.W bit is implied for 64-bit (because 32-bit version is illegal),
        // no need to set it
        if rex != 0 {
            gen.write_u8(0x40 | rex);
        }
        gen.write_u8(0x8f);
        #[allow(clippy::identity_op)]
        gen.write_u8(enc.modrm | (0 << 3));
        gen.write_slice(&enc.buf[..enc.buf_len]);
        gen
    }
}

#[derive(Clone, Copy, Debug)]
pub enum RelJumpTarget {
    Rel8(i8),
    Rel32(i32),
}

impl From<i32> for RelJumpTarget {
    fn from(rel: i32) -> Self {
        match i8::try_from(rel) {
            Ok(rel) => Self::Rel8(rel),
            Err(_) => Self::Rel32(rel),
        }
    }
}

impl Gen {
    pub fn jump(target: impl Into<RelJumpTarget>) -> Gen {
        let target: RelJumpTarget = target.into();
        let mut gen = Gen::default();
        match target {
            RelJumpTarget::Rel8(rel) => {
                gen.buf[0] = 0xeb;
                gen.buf[1] = rel as u8;
                gen.buf_len = 2;
            }
            RelJumpTarget::Rel32(rel) => {
                gen.buf[0] = 0xe9;
                gen.buf[1..5].copy_from_slice(&rel.to_le_bytes());
                gen.buf_len = 5;
            }
        }
        gen
    }

    pub fn call(rel: i32) -> Gen {
        let mut gen = Gen::default();
        gen.buf[0] = 0xe8;
        gen.buf[1..5].copy_from_slice(&rel.to_le_bytes());
        gen.buf_len = 5;
        gen
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Cond {
    O = 0,
    No,
    B,
    Ae,
    E,
    Ne,
    Be,
    A,
    S,
    Ns,
    P,
    Np,
    L,
    Ge,
    Le,
    G,
}

impl Cond {
    pub fn all() -> impl Iterator<Item=Cond> {
        (0..16).map(|i| i.try_into().unwrap())
    }
}

impl TryFrom<u8> for Cond {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Cond::O,
            1 => Cond::No,
            2 => Cond::B,
            3 => Cond::Ae,
            4 => Cond::E,
            5 => Cond::Ne,
            6 => Cond::Be,
            7 => Cond::A,
            8 => Cond::S,
            9 => Cond::Ns,
            10 => Cond::P,
            11 => Cond::Np,
            12 => Cond::L,
            13 => Cond::Ge,
            14 => Cond::Le,
            15 => Cond::G,
            _ => return Err(()),
        })
    }
}

impl Gen {
    pub fn jump_cond(cond: Cond, target: impl Into<RelJumpTarget>) -> Gen {
        let target: RelJumpTarget = target.into();
        let mut gen = Gen::default();
        match target {
            RelJumpTarget::Rel8(rel) => {
                gen.buf[0] = 0x70 | cond as u8;
                gen.buf[1] = rel as u8;
                gen.buf_len = 2;
            }
            RelJumpTarget::Rel32(rel) => {
                gen.buf[0] = 0x0f;
                gen.buf[1] = 0x80 | cond as u8;
                gen.buf[2..6].copy_from_slice(&rel.to_le_bytes());
                gen.buf_len = 6;
            }
        }
        gen
    }
}

#[derive(Clone, Copy, Debug)]
pub enum IndirectJumpTarget {
    Reg(R64),
    Mem(Mem),
}

impl From<R64> for IndirectJumpTarget {
    fn from(r: R64) -> Self {
        Self::Reg(r)
    }
}

impl From<Mem> for IndirectJumpTarget {
    fn from(m: Mem) -> Self {
        Self::Mem(m)
    }
}

impl Gen {
    pub fn jump_indirect(target: impl Into<IndirectJumpTarget>) -> Gen {
        let target: IndirectJumpTarget = target.into();
        let mut gen = Gen::default();
        let enc = match target {
            IndirectJumpTarget::Reg(r) => RmEncoding::from_reg(r as u8),
            IndirectJumpTarget::Mem(m) => {
                assert!(m.size.is_none() || m.size == Some(64));
                RmEncoding::from_mem(m)
            }
        };
        if enc.rex_rxb != 0 {
            gen.write_u8(enc.rex_rxb | 0x40)
        }
        gen.write_u8(0xff);  // opcode
        gen.write_u8(enc.modrm | (4 << 3));
        gen.write_slice(&enc.buf[..enc.buf_len]);
        gen
    }

    pub fn call_indirect(target: impl Into<IndirectJumpTarget>) -> Gen {
        let target: IndirectJumpTarget = target.into();
        let mut gen = Gen::default();
        let enc = match target {
            IndirectJumpTarget::Reg(r) => RmEncoding::from_reg(r as u8),
            IndirectJumpTarget::Mem(m) => {
                assert!(m.size.is_none() || m.size == Some(64));
                RmEncoding::from_mem(m)
            }
        };
        if enc.rex_rxb != 0 {
            gen.write_u8(enc.rex_rxb | 0x40)
        }
        gen.write_u8(0xff);  // opcode
        gen.write_u8(enc.modrm | (2 << 3));
        gen.write_slice(&enc.buf[..enc.buf_len]);
        gen
    }
}

impl Gen {
    pub fn ret() -> Gen {
        let mut gen = Gen::default();
        gen.write_u8(0xc3);
        gen
    }

    pub fn retn(n: u16) -> Gen {
        let mut gen = Gen::default();
        gen.buf[0] = 0xc2;
        gen.buf[1..3].copy_from_slice(&n.to_le_bytes());
        gen.buf_len = 3;
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
            Gen::binop(Binop::Adc, Mem::rip_rel(0x42), R8::Dl).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Adc, Mem::rip_rel(0x42), R16::Dx).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Adc, Mem::rip_rel(0x42), R32::Edx).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Adc, Mem::rip_rel(0x42), R64::Rdx).as_slice());
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
            Gen::binop(Binop::Add, Mem::base(R64::Rcx), R8::Dl).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Add, R8::Dl, Mem::base(R64::Rcx)).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Add, Mem::base(R64::Rcx), R32::Edx).as_slice());
        bytes.extend_from_slice(
            Gen::binop(Binop::Add, R32::Edx, Mem::base(R64::Rcx)).as_slice());
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
            bytes.extend_from_slice(Gen::binop(Binop::Add, Mem::base(r), R32::Eax).as_slice());
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
                    Gen::binop(Binop::Add, Mem::base(base).index_scale(index, 4).disp(0x42), R32::Eax).as_slice());
                expected.push(format!("add    %eax,0x42(%{},%{},4)", base, index));
            }
            let insns = Obj::from_bytes(&bytes).insns();
            assert_eq!(insns.len(), expected.len());
            for (insn, expected) in insns.iter().zip(expected) {
                assert_eq!(insn.text, expected);
            }
        }
    }

    #[test]
    fn mov_imm() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend_from_slice(Gen::mov(R8::Dl, 0x42i8).as_slice());
        bytes.extend_from_slice(Gen::mov(R16::Dx, 0x42i16).as_slice());
        bytes.extend_from_slice(Gen::mov(R32::Edx, 0x42i32).as_slice());
        bytes.extend_from_slice(Gen::mov(R64::Rdx, 0x42i64).as_slice());
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), 4);
        assert_eq!(insns[0].text, "mov    $0x42,%dl");
        assert_eq!(insns[1].text, "mov    $0x42,%dx");
        assert_eq!(insns[2].text, "mov    $0x42,%edx");
        assert_eq!(insns[3].text, "mov    $0x42,%rdx");
    }

    #[test]
    fn mov_to_reg_from_rm() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend_from_slice(Gen::mov(R8::Dl, R8::Al).as_slice());
        bytes.extend_from_slice(Gen::mov(R16::Dx, R16::Ax).as_slice());
        bytes.extend_from_slice(Gen::mov(R8::Dl, Mem::base(R64::Rax)).as_slice());
        bytes.extend_from_slice(Gen::mov(R16::Dx, Mem::base(R64::Rax)).as_slice());
        bytes.extend_from_slice(Gen::mov(R32::Edx, Mem::base(R64::Rax)).as_slice());
        bytes.extend_from_slice(Gen::mov(R64::Rdx, Mem::base(R64::Rax)).as_slice());
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), 6);
        assert_eq!(insns[0].text, "mov    %al,%dl");
        assert_eq!(insns[1].text, "mov    %ax,%dx");
        assert_eq!(insns[2].text, "mov    0x0(%rax),%dl");
        assert_eq!(insns[3].text, "mov    0x0(%rax),%dx");
        assert_eq!(insns[4].text, "mov    0x0(%rax),%edx");
        assert_eq!(insns[5].text, "mov    0x0(%rax),%rdx");
    }

    #[test]
    fn mov_to_rm_from_reg() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend_from_slice(Gen::mov(R8::Al, R8::Dl).as_slice());
        bytes.extend_from_slice(Gen::mov(R16::Ax, R16::Dx).as_slice());
        bytes.extend_from_slice(Gen::mov(Mem::base(R64::Rax), R8::Dl).as_slice());
        bytes.extend_from_slice(Gen::mov(Mem::base(R64::Rax), R16::Dx).as_slice());
        bytes.extend_from_slice(Gen::mov(Mem::base(R64::Rax), R32::Edx).as_slice());
        bytes.extend_from_slice(Gen::mov(Mem::base(R64::Rax), R64::Rdx).as_slice());
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), 6);
        assert_eq!(insns[0].text, "mov    %dl,%al");
        assert_eq!(insns[1].text, "mov    %dx,%ax");
        assert_eq!(insns[2].text, "mov    %dl,0x0(%rax)");
        assert_eq!(insns[3].text, "mov    %dx,0x0(%rax)");
        assert_eq!(insns[4].text, "mov    %edx,0x0(%rax)");
        assert_eq!(insns[5].text, "mov    %rdx,0x0(%rax)");
    }

    #[test]
    fn push() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend_from_slice(Gen::push(R16::Bx).as_slice());
        bytes.extend_from_slice(Gen::push(R16::R9w).as_slice());
        bytes.extend_from_slice(Gen::push(Mem::base(R64::Rax).size(16)).as_slice());
        bytes.extend_from_slice(Gen::push(R64::Rbx).as_slice());
        bytes.extend_from_slice(Gen::push(R64::R9).as_slice());
        bytes.extend_from_slice(Gen::push(Mem::base(R64::Rax).size(64)).as_slice());
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), 6);
        assert_eq!(insns[0].text, "push   %bx");
        assert_eq!(insns[1].text, "push   %r9w");
        assert_eq!(insns[2].text, "pushw  0x0(%rax)");
        assert_eq!(insns[3].text, "push   %rbx");
        assert_eq!(insns[4].text, "push   %r9");
        assert_eq!(insns[5].text, "pushq  0x0(%rax)");
    }

    #[test]
    fn pop() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend_from_slice(Gen::pop(R16::Bx).as_slice());
        bytes.extend_from_slice(Gen::pop(R16::R9w).as_slice());
        bytes.extend_from_slice(Gen::pop(Mem::base(R64::Rax).size(16)).as_slice());
        bytes.extend_from_slice(Gen::pop(R64::Rbx).as_slice());
        bytes.extend_from_slice(Gen::pop(R64::R9).as_slice());
        bytes.extend_from_slice(Gen::pop(Mem::base(R64::Rax).size(64)).as_slice());
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), 6);
        assert_eq!(insns[0].text, "pop    %bx");
        assert_eq!(insns[1].text, "pop    %r9w");
        assert_eq!(insns[2].text, "popw   0x0(%rax)");
        assert_eq!(insns[3].text, "pop    %rbx");
        assert_eq!(insns[4].text, "pop    %r9");
        assert_eq!(insns[5].text, "popq   0x0(%rax)");
    }

    #[test]
    fn jump() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend_from_slice(Gen::jump(-2).as_slice());
        bytes.extend_from_slice(Gen::jump(RelJumpTarget::Rel32(-5)).as_slice());
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), 2);
        assert_eq!(insns[0].text, "jmp    0x0");
        assert_eq!(insns[1].text, "jmpq   0x2");
    }

    #[test]
    fn call() {
        let insns = Obj::from_bytes(Gen::call(-5).as_slice()).insns();
        assert_eq!(insns.len(), 1);
        assert_eq!(insns[0].text, "callq  0x0");
    }

    #[test]
    fn jump_cond() {
        for &rel in &[-1, 1000] {
            let mut bytes = Vec::<u8>::new();
            let mut expected = Vec::new();
            for cond in Cond::all() {
                bytes.extend_from_slice(Gen::jump_cond(cond, rel).as_slice());
                let mut c = format!("{:?}", cond);
                c.make_ascii_lowercase();
                expected.push(format!("j{:<2}    0x{:x}", c, bytes.len() as i32 + rel));
            }
            let insns = Obj::from_bytes(&bytes).insns();
            assert_eq!(insns.len(), expected.len());
            for (insn, expected) in insns.iter().zip(expected) {
                assert_eq!(insn.text, expected);
            }
        }
    }

    #[test]
    fn jump_indirect() {
        let mut bytes = Vec::<u8>::new();
        let mut expected = Vec::new();
        for &(target, target_str) in &[
            (IndirectJumpTarget::Reg(R64::Rax), "%rax"),
            (IndirectJumpTarget::Reg(R64::R9), "%r9"),
            (IndirectJumpTarget::Mem(Mem::rip_rel(0x42)), "0x42(%rip)"),
            (IndirectJumpTarget::Mem(
                Mem::base(R64::Rbx).index_scale(R64::Rdx, 2).disp(0x42)),
             "0x42(%rbx,%rdx,2)"),
        ] {
            bytes.extend_from_slice(Gen::jump_indirect(target).as_slice());
            expected.push(format!("jmpq   *{}", target_str));
        }
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), expected.len());
        for (insn, expected) in insns.iter().zip(expected) {
            assert_eq!(insn.text, expected);
        }
    }

    #[test]
    fn call_indirect() {
        let mut bytes = Vec::<u8>::new();
        let mut expected = Vec::new();
        for &(target, target_str) in &[
            (IndirectJumpTarget::Reg(R64::Rax), "%rax"),
            (IndirectJumpTarget::Reg(R64::R9), "%r9"),
            (IndirectJumpTarget::Mem(Mem::rip_rel(0x42)), "0x42(%rip)"),
            (IndirectJumpTarget::Mem(
                Mem::base(R64::Rbx).index_scale(R64::Rdx, 2).disp(0x42)),
             "0x42(%rbx,%rdx,2)"),
        ] {
            bytes.extend_from_slice(Gen::call_indirect(target).as_slice());
            expected.push(format!("callq  *{}", target_str));
        }
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), expected.len());
        for (insn, expected) in insns.iter().zip(expected) {
            assert_eq!(insn.text, expected);
        }
    }

    #[test]
    fn ret() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend_from_slice(Gen::ret().as_slice());
        bytes.extend_from_slice(Gen::retn(0x9999).as_slice());
        let insns = Obj::from_bytes(&bytes).insns();
        assert_eq!(insns.len(), 2);
        assert_eq!(insns[0].text, "retq");
        assert_eq!(insns[1].text, "retq   $0x9999");
    }
}

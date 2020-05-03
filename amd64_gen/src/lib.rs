#![feature(track_caller)]

use std::convert::{TryFrom, TryInto};

pub mod gen;
#[cfg(test)] mod test_util;

pub trait CodeSink {
    // The code grows backwards.
    // For example
    //     prepend(&[1, 2]);
    //     prepend(&[3]);
    // results in [3, 1, 2].
    fn prepend(&mut self, data: &[u8]);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reg {
    Ax = 0,
    Cx,
    Dx,
    Bx,
    Sp,
    Bp,
    Si,
    Di,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl TryFrom<u8> for Reg {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Reg::Ax,
            1 => Reg::Cx,
            2 => Reg::Dx,
            3 => Reg::Bx,
            4 => Reg::Sp,
            5 => Reg::Bp,
            6 => Reg::Si,
            7 => Reg::Di,
            8 => Reg::R8,
            9 => Reg::R9,
            10 => Reg::R10,
            11 => Reg::R11,
            12 => Reg::R12,
            13 => Reg::R13,
            14 => Reg::R14,
            15 => Reg::R15,
            _ => return Err(()),
        })
    }
}

impl From<Reg> for u8 {
    fn from(r: Reg) -> u8 { r as u8 }
}

impl Reg {
    pub fn all() -> impl Iterator<Item=Self> {
        (0..16).map(|x| x.try_into().unwrap())
    }

    pub fn name32(self) -> &'static str {
        const NAMES: [&str; 16] = [
            "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi",
            "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
        ];
        NAMES[self as usize]
    }

    pub fn name64(self) -> &'static str {
        const NAMES: [&str; 16] = [
            "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
            "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
        ];
        NAMES[self as usize]
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Mem2 {
    base: Reg,
    index_scale: Option<(Reg, u8)>,
    disp: i32,
}

impl Mem2 {
    pub fn base(base: Reg) -> Mem2 {
        Mem2 {
            base,
            index_scale: None,
            disp: 0,
        }
    }

    pub fn index_scale(self, index: Reg, scale: u8) -> Mem2 {
        assert!(index != Reg::Sp);
        Mem2 {
            index_scale: Some((index, scale)),
            ..self
        }
    }

    pub fn disp(self, disp: i32) -> Mem2 {
        Mem2 {
            disp,
            ..self
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum RegOrMem2 {
    Reg(Reg),
    RipRel(i32),
    Mem(Mem2),
}
pub use RegOrMem2::RipRel;

impl From<Reg> for RegOrMem2 {
    fn from(r: Reg) -> Self { Self::Reg(r) }
}

impl From<Mem2> for RegOrMem2 {
    fn from(m: Mem2) -> Self { Self::Mem(m) }
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

#[derive(Clone, Copy, Debug)]
pub enum MulOp {
    Mul = 4,
    Imul = 5,
    Div = 6,
    Idiv = 7,
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

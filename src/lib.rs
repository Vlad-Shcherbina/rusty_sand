#![feature(asm)]
#![feature(naked_functions)]

pub mod interp;
pub mod binutils;
pub mod util;
pub mod amd64_gen;
pub mod exe_buf;
pub mod jit;
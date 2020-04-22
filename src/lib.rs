#![feature(asm)]
#![feature(naked_functions)]
#![feature(slice_fill)]

pub mod interp;
pub mod binutils;
pub mod util;
pub mod amd64_gen;
pub mod exe_buf;
pub mod jit;
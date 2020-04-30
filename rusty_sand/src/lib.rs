#![feature(llvm_asm)]
#![feature(naked_functions)]
#![feature(slice_fill)]

pub mod interp;
pub mod util;
pub mod amd64_gen;
pub mod jit;
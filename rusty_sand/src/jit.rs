use std::io::{Read, Write};
use std::convert::TryFrom;
use exe_buf::ExeBuf;
use amd64_gen::{CodeSink, gen, Reg, Binop, Mem2, Cond, MulOp};
use crate::interp::opcodes;

#[derive(Default)]
pub struct OpStat {
    pub cnt: usize,
    pub code_len: usize,
}

#[derive(Default)]
pub struct Stats {
    pub ops: [OpStat; 14],
    pub compile_time: f64,
}

#[repr(C)]
pub struct State {
    pub finger: u32,
    pub regs: [u32; 8],
    exe_buf: ExeBufCodeSink,
    uncompile_fn_ptr: *const u8,
    jump_locations: Vec<*const u8>,
    arrays: Vec<Vec<u32>>,
    free: Vec<u32>,
    pub stats: Stats,
}

struct ExeBufCodeSink(ExeBuf);

impl ExeBufCodeSink {
    fn new(exe_buf: ExeBuf) -> Self {
        Self(exe_buf)
    }

    fn cur_pos(&self) -> *const u8 {
        self.0.cur_pos()
    }

    fn inner_mut(&mut self) -> &mut ExeBuf {
        &mut self.0
    }
}

impl CodeSink for ExeBufCodeSink {
    fn prepend(&mut self, data: &[u8]) {
        self.inner_mut().push(data);
    }
}

impl State {
    pub fn new(prog: Vec<u32>) -> State {
        let mut exe_buf = ExeBufCodeSink::new(ExeBuf::reserve(1 << 30));

        // call [rbx].uncompile(rdx)
        gen::ret(&mut exe_buf);
        for &r in VOLATILE_REGS {  // TODO: unroll
            if r != Reg::Ax && r != Reg::Cx && r != Reg::Dx {
                gen::pop64(&mut exe_buf, r);
            }
        }
        gen::call_indirect(&mut exe_buf, Reg::Ax);
        gen::movabs64_imm(&mut exe_buf, Reg::Ax, State::uncompile as usize as i64);
        gen::mov64_r_rm(&mut exe_buf, Reg::Cx, Reg::Bx);
        for &r in VOLATILE_REGS.iter().rev() {
            if r != Reg::Ax && r != Reg::Cx && r != Reg::Dx {
                gen::push64(&mut exe_buf, r);
            }
        }
        let uncompile_fn_ptr = exe_buf.cur_pos() as *const u8;

        let jt = jit_trampoline as usize as *const u8;
        State {
            finger: 0,
            regs: [0; 8],
            uncompile_fn_ptr,
            exe_buf,
            jump_locations: vec![jt; prog.len()],
            arrays: vec![prog],
            free: vec![],
            stats: Stats::default(),
        }
    }
}

#[naked]
unsafe fn jit_trampoline() {
    llvm_asm!("
    // push all win64 volatile registers
    push rax
    push rcx
    push rdx
    push r8
    push r9
    push r10
    push r11

    mov rcx, rbx  // arg1 = self
    mov edx, eax  // finger
    sub rsp, 0x20
    call rbp  // state.compile(finger)
    add rsp, 0x20

    // pop all win64 volatile registers
    pop r11
    pop r10
    pop r9
    pop r8
    pop rdx
    pop rcx
    pop rax

    // resume execution of the compiled code
    jmp [rsi + 8 * rax]
    "
    : : "{rbp}"(State::compile as usize) : : "intel");
}

extern "win64" fn output(c: u32) {
    print!("{}", u8::try_from(c).unwrap() as char);
    std::io::stdout().flush().unwrap();
}

extern "win64" fn input() -> u32 {
    let mut ch = 0u8;
    match std::io::stdin().read_exact(std::slice::from_mut(&mut ch)) {
        Ok(()) => ch as u32,
        Err(e) => if e.kind() == std::io::ErrorKind::UnexpectedEof {
            !0
        } else {
            panic!("{}", e)
        }
    }
}

extern "win64" fn fail(s: *const std::os::raw::c_char) {
    let s = unsafe { std::ffi::CStr::from_ptr(s) };
    println!("fail: {}", s.to_str().unwrap());
    unsafe {
        llvm_asm!("int3");
    }
    std::process::exit(1);
}

fn fail_code(buf: &mut ExeBufCodeSink, s: &'static str) {
    assert!(s.ends_with('\0'));
    gen::call_indirect(buf, Reg::Ax);

    // ensure 16-byte stack alignment
    gen::binop64_imm(buf, Binop::Sub, Reg::Sp, 0x80);
    gen::binop64_imm(buf, Binop::And, Reg::Sp, -16);

    gen::movabs64_imm(buf, Reg::Ax, fail as usize as i64);
    gen::movabs64_imm(buf, Reg::Cx, s.as_ptr() as usize as i64);
}

fn is_fallthrough(cmd: u32) -> bool {
    let op = cmd >> 28;
    op != opcodes::HALT && op != opcodes::LOAD_PROGRAM
}

const VEC_PTR_OFFSET: usize = 0;
#[cfg(test)] const VEC_CAPACITY_OFFSET: usize = 8;
const VEC_LEN_OFFSET: usize = 16;

/// Registers that should be saved by the caller in the win64 calling convention.
const VOLATILE_REGS: &[Reg] = &[Reg::Ax, Reg::Cx, Reg::Dx, Reg::R8, Reg::R9, Reg::R10, Reg::R11];


impl State {
    fn compile_insn(&mut self, pos: usize) {
        let insn = self.arrays[0][pos];
        let op = insn >> 28;
        self.stats.ops[op as usize].cnt += 1;
        let end_ptr = self.exe_buf.cur_pos();
        let buf = &mut self.exe_buf;
        if op == opcodes::ORTHOGRAPHY {
            let a = ((insn >> 25) & 7) as u8;
            let a = Reg::try_from(8 + a).unwrap();
            let imm = insn & ((1 << 25) - 1);
            gen::mov32_imm(buf, a, imm as i32);
            self.stats.ops[op as usize].code_len += end_ptr as usize - buf.cur_pos() as usize;
            return;
        }
        let a = ((insn >> 6) & 7) as u8;
        let b = ((insn >> 3) & 7) as u8;
        let c = (insn & 7) as u8;
        let a = Reg::try_from(8 + a).unwrap();
        let b = Reg::try_from(8 + b).unwrap();
        let c = Reg::try_from(8 + c).unwrap();
        match op {
            opcodes::CMOVE => {
                let skip = buf.cur_pos();
                gen::mov32_r_rm(buf, a, b);
                let rel = i32::try_from(skip as usize - buf.cur_pos() as usize).unwrap();
                gen::jmp_cond(buf, Cond::E, rel);
                gen::binop32_imm(buf, Binop::Cmp, c, 0);
            }
            opcodes::ARRAY_INDEX => {
                // TODO: bounds check

                // lea rax, [b + 2 * b]
                // mov rcx, self.arrays.ptr
                // mov rax, [rcx + 8 * rax + VEC_PTR_OFFSET]
                // mov a, [rax + 4 * c]
                assert_eq!(std::mem::size_of::<Vec<u32>>(), 24);
                gen::mov32_r_rm(buf, a, Mem2::base(Reg::Ax).index_scale(c, 4));
                gen::mov64_r_rm(buf, Reg::Ax,
                    Mem2::base(Reg::Cx).index_scale(Reg::Ax, 8)
                    .disp(i32::try_from(VEC_PTR_OFFSET).unwrap()));
                gen::mov64_r_rm(buf, Reg::Cx,
                    Mem2::base(Reg::Bx).disp(i32::try_from(
                        memoffset::offset_of!(State, arrays) + VEC_PTR_OFFSET
                    ).unwrap()));
                gen::lea64(buf, Reg::Ax, Mem2::base(b).index_scale(b, 2));
            }
            opcodes::ARRAY_AMENDMENT => {
                // TODO: bounds check

                let skip = buf.cur_pos();

                // jump to the next instruction
                gen::jmp_indirect(buf, Mem2::base(Reg::Si).index_scale(Reg::Ax, 8));
                gen::mov32_imm(buf, Reg::Ax, i32::try_from(pos + 1).unwrap());

                // call self.uncompile(b)
                gen::call_rel(buf, i32::try_from(self.uncompile_fn_ptr as usize - buf.cur_pos() as usize).unwrap());
                gen::mov64_r_rm(buf, Reg::Dx, b);  // TODO could it be mov32?

                // if jump_targets[b] == jit_trampoline goto skip
                gen::jmp_cond(buf, Cond::E,
                    i32::try_from(skip as usize - buf.cur_pos() as usize).unwrap());
                gen::binop64_rm_r(buf, Binop::Cmp,
                    Mem2::base(Reg::Si).index_scale(b, 8),
                    Reg::Ax,
                );
                gen::movabs64_imm(buf, Reg::Ax, jit_trampoline as usize as i64);

                // if a != 0 goto skip
                gen::jmp_cond(buf, Cond::Ne,
                    i32::try_from(skip as usize - buf.cur_pos() as usize).unwrap());
                gen::binop64_imm(buf, Binop::Cmp, a, 0);  // TODO: could it be binop32?

                // self.arrays[a][b] <- c
                //    or
                // lea rax, [a + 2 * a]
                // mov rcx, self.arrays.ptr
                // mov rax, [rcx + 8 * rax + VEC_PTR_OFFSET]
                // mov [rax + 4 * b], c
                assert_eq!(std::mem::size_of::<Vec<u32>>(), 24);
                gen::mov32_rm_r(buf, Mem2::base(Reg::Ax).index_scale(b, 4), c);
                gen::mov64_r_rm(buf, Reg::Ax,
                    Mem2::base(Reg::Cx).index_scale(Reg::Ax, 8).disp(i32::try_from(VEC_PTR_OFFSET).unwrap()));
                gen::mov64_r_rm(buf, Reg::Cx,
                    Mem2::base(Reg::Bx).disp(i32::try_from(
                        memoffset::offset_of!(State, arrays) + VEC_PTR_OFFSET).unwrap()));
                gen::lea64(buf, Reg::Ax, Mem2::base(a).index_scale(a, 2));
            }
            opcodes::ADDITION => {
                gen::mov32_r_rm(buf, a, Reg::Ax);
                gen::binop32_r_rm(buf, Binop::Add, Reg::Ax, c);
                gen::mov32_r_rm(buf, Reg::Ax, b);
            }
            opcodes::MULTIPLICATION => {
                gen::pop64(buf, Reg::Dx);
                gen::mov32_r_rm(buf, a, Reg::Ax);
                gen::mul_op32(buf, MulOp::Mul, c);
                gen::mov32_r_rm(buf, Reg::Ax, b);
                gen::push64(buf, Reg::Dx);
            }
            opcodes::DIVISION => {
                // TODO: maybe explicitly fail on division by zero?
                gen::pop64(buf, Reg::Dx);
                gen::mov32_r_rm(buf, a, Reg::Ax);
                gen::mul_op32(buf, MulOp::Div, c);
                gen::mov32_r_rm(buf, Reg::Ax, b);
                gen::mov32_imm(buf, Reg::Dx, 0);  // TODO: xor edx, edx
                gen::push64(buf, Reg::Dx);
            }
            opcodes::NOT_AND => {
                gen::mov32_r_rm(buf, a, Reg::Ax);
                gen::binop32_imm(buf, Binop::Xor, Reg::Ax, -1);
                gen::binop32_r_rm(buf, Binop::And, Reg::Ax, c);
                gen::mov32_r_rm(buf, Reg::Ax, b);
            }
            opcodes::HALT => {
                // ret from 'call jump_locations[finger]' in State::run()
                gen::ret(buf);

                // self.finger <- pos + 1
                gen::mov32_imm(buf,
                    Mem2::base(Reg::Bx).disp(i32::try_from(memoffset::offset_of!(State, finger)).unwrap()),
                    i32::try_from(pos + 1).unwrap());
            }
            opcodes::ALLOCATION => {
                // b <- call self.allocation(c)
                gen::mov32_r_rm(buf, b, Reg::Ax);
                for &r in VOLATILE_REGS {
                    if r != Reg::Ax {
                        gen::pop64(buf, r);
                    } else {
                        gen::binop64_imm(buf, Binop::Add, Reg::Sp, 8);
                    }
                }
                gen::call_indirect(buf, Reg::Ax);
                gen::movabs64_imm(buf, Reg::Ax, State::allocation as usize as i64);
                gen::mov32_r_rm(buf, Reg::Dx, c);
                gen::mov64_r_rm(buf, Reg::Cx, Reg::Bx);
                for &r in VOLATILE_REGS.iter().rev() {
                    if r != Reg::Ax {
                        gen::push64(buf, r);
                    } else {
                        gen::binop64_imm(buf, Binop::Sub, Reg::Sp, 8);
                    }
                }
            }
            opcodes::ABANDONMENT => {
                for &r in VOLATILE_REGS {
                    gen::pop64(buf, r);
                }
                gen::call_indirect(buf, Reg::Ax);
                gen::movabs64_imm(buf, Reg::Ax, State::abandonment as usize as i64);
                gen::mov32_r_rm(buf, Reg::Dx, c);
                gen::mov64_r_rm(buf, Reg::Cx, Reg::Bx);
                for &r in VOLATILE_REGS.iter().rev() {
                    gen::push64(buf, r);
                }
            }
            opcodes::OUTPUT => {
                for &r in VOLATILE_REGS {
                    gen::pop64(buf, r);
                }
                gen::call_indirect(buf, Reg::Ax);
                gen::movabs64_imm(buf, Reg::Ax, output as usize as i64);
                gen::mov32_r_rm(buf, Reg::Cx, c);
                for &r in VOLATILE_REGS.iter().rev() {
                    gen::push64(buf, r);
                }
            }
            opcodes::INPUT => {
                gen::mov32_r_rm(buf, c, Reg::Ax);
                for &r in VOLATILE_REGS {
                    if r != Reg::Ax {
                        gen::pop64(buf, r);
                    } else {
                        gen::binop64_imm(buf, Binop::Add, Reg::Sp, 8);
                    }
                }
                gen::call_indirect(buf, Reg::Ax);
                gen::movabs64_imm(buf, Reg::Ax, input as usize as i64);
                for &r in VOLATILE_REGS.iter().rev() {
                    if r != Reg::Ax {
                        gen::push64(buf, r);
                    } else {
                        gen::binop64_imm(buf, Binop::Sub, Reg::Sp, 8);
                    }
                }
            }
            opcodes::LOAD_PROGRAM => {
                // TODO: assert regs[b] == 0
                // TODO: assert regs[c] < jump_locations.len()

                gen::jmp_indirect(buf, Mem2::base(Reg::Si).index_scale(c, 8));
                gen::mov64_r_rm(buf, Reg::Ax, c);

                let no_fail = buf.cur_pos();
                fail_code(buf, "LOAD_PROGRAM: c >= arrays[0].len()\0");
                gen::jmp_cond(buf, Cond::B,
                    i32::try_from(no_fail as usize - buf.cur_pos() as usize).unwrap());
                gen::binop64_r_rm(buf, Binop::Cmp, c, Reg::Di);

                let no_switch_code = buf.cur_pos();

                // rsi <- jump_locations
                gen::mov64_r_rm(buf, Reg::Si,
                    Mem2::base(Reg::Bx).disp(i32::try_from(
                        memoffset::offset_of!(State, jump_locations) + VEC_PTR_OFFSET).unwrap()));
                // rdi <- jump_locations.len
                gen::mov64_r_rm(buf, Reg::Di,
                    Mem2::base(Reg::Bx).disp(i32::try_from(
                        memoffset::offset_of!(State, jump_locations) + VEC_LEN_OFFSET).unwrap()));

                // call self.swith_code(b)
                for &r in VOLATILE_REGS {
                    gen::pop64(buf, r);
                }
                gen::call_indirect(buf, Reg::Ax);
                gen::movabs64_imm(buf, Reg::Ax, State::switch_code as usize as i64);
                gen::mov64_r_rm(buf, Reg::Dx, b);
                gen::mov64_r_rm(buf, Reg::Cx, Reg::Bx);
                for &r in VOLATILE_REGS.iter().rev() {
                    gen::push64(buf, r);
                }

                // if b != 0 goto no_switch_code
                gen::jmp_cond(buf, Cond::E, i32::try_from(
                    no_switch_code as usize - buf.cur_pos() as usize).unwrap());
                gen::binop64_imm(buf, Binop::Cmp, b, 0);  // TODO: could be binop32
            }
            _ => panic!("op: {}", op),
        }
        self.stats.ops[op as usize].code_len += end_ptr as usize - buf.cur_pos() as usize;
    }

    extern "win64" fn compile(&mut self, finger: u32) {
        let timer = std::time::Instant::now();
        let finger = finger as usize;
        let mut end = finger;
        assert_eq!(self.jump_locations[end], jit_trampoline as *const u8);
        while is_fallthrough(self.arrays[0][end]) {
            end += 1;
        }
        // println!("State::compile({}..={})", finger, end);
        // for i in finger..end + 1 {
        //     println!("{:>5}: {}", i, interp::insn_to_string(self.arrays[0][i]));
        // }
        for i in (finger..end + 1).rev() {
            // let zzz = self.exe_buf.cur_pos();
            self.compile_insn(i);
            // let yyy = self.exe_buf.cur_pos();
            // let code = unsafe {std::slice::from_raw_parts(yyy, zzz as usize - yyy as usize) };
            // dbg!(crate::binutils::Obj::from_bytes(code).insns());
            // crate::binutils::Obj::from_bytes(code).insns();
            self.jump_locations[i as usize] = self.exe_buf.cur_pos();
        }
        self.stats.compile_time += timer.elapsed().as_secs_f64();
    }

    extern "win64" fn uncompile(&mut self, finger: u32) {
        let finger = finger as usize;
        let jt = jit_trampoline as *const u8;
        assert!(self.jump_locations[finger] != jt);
        let mut start = finger;
        // TODO: in principle it's enough to backtrack up to the first
        // non-fallthrough instruction, but we should look
        // at the instructions before amendment.
        // Currently uncompile() is called after amendment,
        // so we have no way of knowing if the instruction was fallthrough.
        while start > 0 && self.jump_locations[start - 1] != jt
            /* && is_fallthrough(self.arrays[0][start - 1]) */ {
            start -= 1;
        }
        self.jump_locations[start..finger + 1].fill(jt);
        // println!("State::uncompile({}..={})", start, finger);
    }

    extern "win64" fn switch_code(&mut self, idx: u32) {
        assert!(idx != 0);
        // println!("State::switch_code({})", idx);
        self.arrays[0] = self.arrays[idx as usize].clone();
        self.jump_locations = vec![jit_trampoline as *const u8; self.arrays[0].len()];
    }

    extern "win64" fn allocation(&mut self, size: u32) -> u32 {
        // println!("State::allocation({})", size);
        let arr = vec![0u32; size as usize];
        let idx = match self.free.pop() {
            Some(i) => {
                assert!(self.arrays[i as usize].is_empty());
                self.arrays[i as usize] = arr;
                i
            }
            None => {
                self.arrays.push(arr);
                (self.arrays.len() - 1) as u32
            }
        };
        assert!(idx != 0);
        idx
    }

    extern "win64" fn abandonment(&mut self, idx: u32) {
        // println!("State::abandonment({})", idx);
        self.arrays[idx as usize] = vec![];
        self.free.push(idx);
    }

    pub fn run(&mut self) {
        assert!((self.finger as usize) < self.jump_locations.len());
        // regs[] -> r08..r15
        // jump_locations, len -> (rsi, rdi)
        // arrays -> some reg TODO
        // jit_trampoline -> rbp (for fast comparisons)
        // finger -> rax
        // self -> rbx
        //
        // call jump_locations[finger]
        //
        // r08..r15 -> regs[]
        unsafe {
            llvm_asm!("
            call [rsi + 8 * rax]
            "
            :
              "={r8d}"(self.regs[0]),
              "={r9d}"(self.regs[1]),
              "={r10d}"(self.regs[2]),
              "={r11d}"(self.regs[3]),
              "={r12d}"(self.regs[4]),
              "={r13d}"(self.regs[5]),
              "={r14d}"(self.regs[6]),
              "={r15d}"(self.regs[7])
            :
              "{rbx}"(self as *mut _),
              "{rsi}"(self.jump_locations.as_ptr()),
              "{rdi}"(self.jump_locations.len()),
              "{rbp}"(jit_trampoline as usize),
              "{rax}"(self.finger),
              "{r8d}"(self.regs[0]),
              "{r9d}"(self.regs[1]),
              "{r10d}"(self.regs[2]),
              "{r11d}"(self.regs[3]),
              "{r12d}"(self.regs[4]),
              "{r13d}"(self.regs[5]),
              "{r14d}"(self.regs[6]),
              "{r15d}"(self.regs[7])
            : "memory", "cc", "rcx", "rdx"
            : "intel");
        }
        // println!("done");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_vec_layout<T: Clone>(elem: T) {
        let mut xs = Vec::<T>::with_capacity(10);
        xs.push(elem.clone());
        xs.push(elem);
        let ptr = &xs as *const _ as *const u8;
        #[allow(clippy::cast_ptr_alignment)]
        unsafe {
            assert_eq!((ptr.add(VEC_PTR_OFFSET) as *const u64).read(), &xs[0] as *const _ as u64);
            assert_eq!((ptr.add(VEC_CAPACITY_OFFSET) as *const u64).read(), 10);
            assert_eq!((ptr.add(VEC_LEN_OFFSET) as *const u64).read(), 2);
        }

    }

    #[test]
    fn vec_layout() {
        // From Vec documentation:
        // "Vec is and always will be a (pointer, capacity, length) triplet.
        // No more, no less. The order of these fields is completely
        // unspecified.""
        // So we test it just in case.
        check_vec_layout::<u32>(0);
        check_vec_layout::<*const u8>(std::ptr::null());
        check_vec_layout::<Vec<u32>>(vec![]);
    }

    #[test]
    fn halt() {
        let mut s = State::new(vec![opcodes::HALT << 28]);
        s.run();
        assert_eq!(s.finger, 1);
        s.finger = 0;
        s.run();
        assert_eq!(s.finger, 1);
    }

    #[test]
    fn constant() {
        for reg in 0..8 {
            let mut s = State::new(vec![
                opcodes::ORTHOGRAPHY << 28 | reg << 25 | 42,
                opcodes::HALT << 28,
            ]);
            s.run();
            let mut expected = [0; 8];
            expected[reg as usize] = 42;
            assert_eq!(s.regs, expected);
            assert_eq!(s.finger, 2);
        }
    }

    #[test]
    fn load_program() {
        for dst in 2..5 {
            for reg in 0..8 {
                let mut s = State::new(vec![
                    opcodes::ORTHOGRAPHY << 28 | reg << 25 | dst,
                    opcodes::LOAD_PROGRAM << 28 | (reg ^ 1) << 3 | reg,
                    opcodes::HALT << 28,
                    opcodes::HALT << 28,
                    opcodes::HALT << 28,
                ]);
                s.run();
                assert_eq!(s.finger, dst + 1);
            }
        }
    }

    #[test]
    fn load_program_switch_code() {
        let mut s = State::new(vec![
            opcodes::LOAD_PROGRAM << 28 | 0o012,
        ]);
        s.arrays.push(vec![
            opcodes::HALT << 28,
            opcodes::HALT << 28,
            opcodes::ORTHOGRAPHY << 28 | 42,
            opcodes::HALT << 28,
        ]);
        s.regs[1] = 1;
        s.regs[2] = 2;
        s.run();
        assert_eq!(s.finger, 4);
        assert_eq!(s.arrays[0], s.arrays[1]);
        assert_eq!(s.regs[0], 42);
    }

    #[test]
    fn cmove() {
        let mut s = State::new(vec![
            opcodes::CMOVE << 28 | 0o012,
            opcodes::HALT << 28,
        ]);
        s.regs[0] = 50;
        s.regs[1] = 60;
        s.regs[2] = 70;
        s.run();
        assert_eq!(s.finger, 2);
        assert_eq!(s.regs, [60, 60, 70, 0, 0, 0, 0, 0]);

        s.finger = 0;
        s.regs[0] = 50;
        s.regs[1] = 60;
        s.regs[2] = 0;
        s.run();
        assert_eq!(s.finger, 2);
        assert_eq!(s.regs, [50, 60, 0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn arithmetic() {
        let mut s = State::new(vec![
            opcodes::ADDITION << 28 | 0o201,
            opcodes::MULTIPLICATION << 28 | 0o301,
            opcodes::DIVISION << 28 | 0o401,
            opcodes::NOT_AND << 28 | 0o501,
            opcodes::HALT << 28,
        ]);
        s.regs[0] = 30;
        s.regs[1] = 4;
        s.run();
        assert_eq!(s.finger, 5);
        assert_eq!(s.regs[2], 30 + 4);
        assert_eq!(s.regs[3], 30 * 4);
        assert_eq!(s.regs[4], 30 / 4);
        assert_eq!(s.regs[5], !(30 & 4));
    }

    #[test]
    fn inplace_binop() {
        let mut s = State::new(vec![
            opcodes::ADDITION << 28 | 0o112,
            opcodes::HALT << 28,
        ]);
        s.regs[1] = 1234;
        s.regs[2] = 5678;
        s.run();
        assert_eq!(s.regs[1], 1234 + 5678);
        assert_eq!(s.regs[2], 5678);

        let mut s = State::new(vec![
            opcodes::ADDITION << 28 | 0o121,
            opcodes::HALT << 28,
        ]);
        s.regs[1] = 1234;
        s.regs[2] = 5678;
        s.run();
        assert_eq!(s.regs[1], 5678 + 1234);
        assert_eq!(s.regs[2], 5678);

        let mut s = State::new(vec![
            opcodes::NOT_AND << 28 | 0o112,
            opcodes::HALT << 28,
        ]);
        s.regs[1] = 1234;
        s.regs[2] = 5678;
        s.run();
        assert_eq!(s.regs[1], !(1234 & 5678));
        assert_eq!(s.regs[2], 5678);

        let mut s = State::new(vec![
            opcodes::NOT_AND << 28 | 0o121,
            opcodes::HALT << 28,
        ]);
        s.regs[1] = 1234;
        s.regs[2] = 5678;
        s.run();
        assert_eq!(s.regs[1], !(5678 & 1234));
        assert_eq!(s.regs[2], 5678);
    }

    #[test]
    fn array_index() {
        let mut s = State::new(vec![
            opcodes::ARRAY_INDEX << 28 | 0o123,
            opcodes::HALT << 28,
        ]);
        s.arrays.push(vec![0, 100, 200, 300]);
        s.regs[2] = 1;
        s.regs[3] = 3;
        s.run();
        assert_eq!(s.regs[1], 300);
    }

    #[test]
    fn array_amendment() {
        let mut s = State::new(vec![
            opcodes::ARRAY_AMENDMENT << 28 | 0o123,
            opcodes::HALT << 28,
            0,
        ]);
        s.arrays.push(vec![0; 4]);
        s.regs[1] = 1;
        s.regs[2] = 2;
        s.regs[3] = 42;
        s.run();
        assert_eq!(s.arrays[1], [0, 0, 42, 0]);

        s.finger = 0;
        s.regs[1] = 0;
        s.regs[2] = 2;
        s.regs[3] = 100;
        s.run();
        assert_eq!(s.arrays[0][2], 100);
    }

    #[test]
    fn array_amendment_with_recompilation() {
        let mut s = State::new(vec![
            opcodes::ARRAY_AMENDMENT << 28 | 0o123,
            opcodes::HALT << 28,
            opcodes::HALT << 28,
        ]);
        s.regs[1] = 0;
        s.regs[2] = 1;
        s.regs[3] = opcodes::ORTHOGRAPHY << 28 | 42;
        s.run();
        assert_eq!(s.finger, 3);
        assert_eq!(s.arrays[0][1], opcodes::ORTHOGRAPHY << 28 | 42);
        assert_eq!(s.regs[0], 42);
    }

    #[test]
    fn allocation() {
        let mut s = State::new(vec![
            opcodes::ALLOCATION << 28 | 0o001,
            opcodes::ALLOCATION << 28 | 0o063,
            opcodes::HALT << 28,
        ]);
        s.regs[1] = 5;
        s.regs[3] = 7;
        s.run();
        assert_eq!(s.arrays.len(), 3);
        assert_eq!(s.arrays[1], [0; 5]);
        assert_eq!(s.arrays[2], [0; 7]);
        assert_eq!(s.regs[0], 1);
        assert_eq!(s.regs[6], 2);
    }

    #[test]
    fn abandonment() {
        let mut s = State::new(vec![
            opcodes::ABANDONMENT << 28 | 0o007,
            opcodes::HALT << 28,
        ]);
        s.regs[7] = 1;
        s.arrays.push(vec![0, 0]);
        s.run();
        assert_eq!(s.arrays[1].capacity(), 0);
        assert_eq!(s.free, [1]);
    }

    #[test]
    fn output() {
        let mut code = Vec::<u32>::new();
        for &c in b"hello, world!\n" {
            code.push(opcodes::ORTHOGRAPHY << 28 | 3 << 25 | u32::from(c));
            code.push(opcodes::OUTPUT << 28 | 0o003);
        }
        code.push(opcodes::HALT << 28);
        let mut s = State::new(code);
        s.run();
    }
}

use std::io::{Read, Write};
use std::convert::TryFrom;
use exe_buf::ExeBuf;
use crate::amd64_gen::{Gen, R32, R64, Binop, Mem, Cond, MulOp};
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
    exe_buf: ExeBuf,
    uncompile_fn_ptr: *const u8,
    jump_locations: Vec<*const u8>,
    arrays: Vec<Vec<u32>>,
    free: Vec<u32>,
    pub stats: Stats,
}

impl State {
    pub fn new(prog: Vec<u32>) -> State {
        let mut exe_buf = ExeBuf::reserve(1 << 30);

        // call [rbx].uncompile(rdx)
        exe_buf.push(Gen::ret().as_slice());
        for &r in VOLATILE_REGS {
            if r != R64::Rax && r != R64::Rcx && r != R64::Rdx {
                exe_buf.push(Gen::pop(r).as_slice());
            }
        }
        exe_buf.push(Gen::call_indirect(R64::Rax).as_slice());
        exe_buf.push(Gen::mov(R64::Rax, State::uncompile as usize as i64).as_slice());
        exe_buf.push(Gen::mov(R64::Rcx, R64::Rbx).as_slice());
        for &r in VOLATILE_REGS.iter().rev() {
            if r != R64::Rax && r != R64::Rcx && r != R64::Rdx {
                exe_buf.push(Gen::push(r).as_slice());
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

extern "win64" fn halt() {
    // println!("halt");
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

fn fail_code(s: &'static str) -> Vec<u8> {
    let mut buf = Vec::with_capacity(50);
    buf.extend_from_slice(Gen::mov(R64::Rcx, s.as_ptr() as usize as i64).as_slice());
    buf.extend_from_slice(Gen::mov(R64::Rax, fail as usize as i64).as_slice());

    // ensure 16-byte stack alignment
    buf.extend_from_slice(Gen::binop(Binop::And, R64::Rsp, -16i64).as_slice());
    buf.extend_from_slice(Gen::binop(Binop::Sub, R64::Rsp, 0x80i64).as_slice());

    buf.extend_from_slice(Gen::call_indirect(R64::Rax).as_slice());
    buf
}

fn is_fallthrough(cmd: u32) -> bool {
    let op = cmd >> 28;
    op != opcodes::HALT && op != opcodes::LOAD_PROGRAM
}

const VEC_PTR_OFFSET: usize = 0;
#[cfg(test)] const VEC_CAPACITY_OFFSET: usize = 8;
const VEC_LEN_OFFSET: usize = 16;

/// Registers that should be saved by the caller in the win64 calling convention.
const VOLATILE_REGS: &[R64] = &[R64::Rax, R64::Rcx, R64::Rdx, R64::R8, R64::R9, R64::R10, R64::R11];

impl State {
    fn compile_insn(&mut self, pos: usize) {
        let insn = self.arrays[0][pos];
        let op = insn >> 28;
        self.stats.ops[op as usize].cnt += 1;
        let end_ptr = self.exe_buf.cur_pos();
        if op == opcodes::ORTHOGRAPHY {
            let a = ((insn >> 25) & 7) as usize;
            let imm = insn & ((1 << 25) - 1);
            self.exe_buf.push(Gen::mov(R32::try_from(8 + a as u8).unwrap(), imm as i32).as_slice());
            self.stats.ops[op as usize].code_len += end_ptr as usize - self.exe_buf.cur_pos() as usize;
            return;
        }
        let a = ((insn >> 6) & 7) as usize;
        let b = ((insn >> 3) & 7) as usize;
        let c = (insn & 7) as usize;
        match op {
            opcodes::CMOVE => {
                let a = R32::try_from(8 + a as u8).unwrap();
                let b = R32::try_from(8 + b as u8).unwrap();
                let c = R32::try_from(8 + c as u8).unwrap();
                let skip = self.exe_buf.cur_pos();
                self.exe_buf.push(Gen::mov(a, b).as_slice());
                self.exe_buf.push(Gen::jump_cond(Cond::E,
                    i32::try_from(skip as usize - self.exe_buf.cur_pos() as usize).unwrap(),
                ).as_slice());
                self.exe_buf.push(Gen::binop(Binop::Cmp, c, 0i32).as_slice());
            }
            opcodes::ARRAY_INDEX => {
                // TODO: bounds check
                let a = R32::try_from(8 + a as u8).unwrap();
                let b = R64::try_from(8 + b as u8).unwrap();
                let c = R64::try_from(8 + c as u8).unwrap();

                // lea rax, [b + 2 * b]
                // mov rcx, self.arrays.ptr
                // mov rax, [rcx + 8 * rax + VEC_PTR_OFFSET]
                // mov a, [rax + 4 * c]

                assert_eq!(std::mem::size_of::<Vec<u32>>(), 24);
                self.exe_buf.push(Gen::mov(
                    a,
                    Mem::base(R64::Rax).index_scale(c, 4),
                ).as_slice());
                self.exe_buf.push(Gen::mov(
                    R64::Rax,
                    Mem::base(R64::Rcx).index_scale(R64::Rax, 8).disp(i32::try_from(VEC_PTR_OFFSET).unwrap()),
                ).as_slice());
                self.exe_buf.push(Gen::mov(
                    R64::Rcx,
                    Mem::base(R64::Rbx).disp(i32::try_from(
                        memoffset::offset_of!(State, arrays) + VEC_PTR_OFFSET).unwrap()),
                ).as_slice());
                self.exe_buf.push(Gen::lea(R64::Rax, Mem::base(b).index_scale(b, 2)).as_slice());
            }
            opcodes::ARRAY_AMENDMENT => {
                // TODO: bounds check
                let a = R64::try_from(8 + a as u8).unwrap();
                let b = R64::try_from(8 + b as u8).unwrap();
                let c = R32::try_from(8 + c as u8).unwrap();

                let skip = self.exe_buf.cur_pos();

                // jump to the next instruction
                self.exe_buf.push(Gen::jump_indirect(Mem::base(R64::Rsi).index_scale(R64::Rax, 8)).as_slice());
                self.exe_buf.push(Gen::mov(R32::Eax, i32::try_from(pos + 1).unwrap()).as_slice());

                // call self.uncompile(b)
                self.exe_buf.push(Gen::call(i32::try_from(
                    self.uncompile_fn_ptr as usize - self.exe_buf.cur_pos() as usize
                ).unwrap()).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rdx, b).as_slice());

                // if jump_targets[b] == jit_trampoline goto skip
                self.exe_buf.push(Gen::jump_cond(
                    Cond::E,
                    i32::try_from(skip as usize - self.exe_buf.cur_pos() as usize).unwrap(),
                ).as_slice());
                self.exe_buf.push(Gen::binop(Binop::Cmp,
                    Mem::base(R64::Rsi)
                        .index_scale(b, 8),
                    R64::Rax,
                ).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rax, jit_trampoline as usize as i64).as_slice());

                // if a != 0 goto skip
                self.exe_buf.push(Gen::jump_cond(
                    Cond::Ne,
                    i32::try_from(skip as usize - self.exe_buf.cur_pos() as usize).unwrap(),
                ).as_slice());
                self.exe_buf.push(Gen::binop(Binop::Cmp, a, 0i64).as_slice());

                // self.arrays[a][b] <- c
                //    or
                // lea rax, [a + 2 * a]
                // mov rcx, self.arrays.ptr
                // mov rax, [rcx + 8 * rax + VEC_PTR_OFFSET]
                // mov [rax + 4 * b], c
                assert_eq!(std::mem::size_of::<Vec<u32>>(), 24);
                self.exe_buf.push(Gen::mov(
                    Mem::base(R64::Rax).index_scale(b, 4),
                    c,
                ).as_slice());
                self.exe_buf.push(Gen::mov(
                    R64::Rax,
                    Mem::base(R64::Rcx).index_scale(R64::Rax, 8).disp(i32::try_from(VEC_PTR_OFFSET).unwrap()),
                ).as_slice());
                self.exe_buf.push(Gen::mov(
                    R64::Rcx,
                    Mem::base(R64::Rbx).disp(i32::try_from(
                        memoffset::offset_of!(State, arrays) + VEC_PTR_OFFSET).unwrap()),
                ).as_slice());
                self.exe_buf.push(Gen::lea(R64::Rax, Mem::base(a).index_scale(a, 2)).as_slice());
            }
            opcodes::ADDITION => {
                let a = R32::try_from(8 + a as u8).unwrap();
                let b = R32::try_from(8 + b as u8).unwrap();
                let c = R32::try_from(8 + c as u8).unwrap();
                self.exe_buf.push(Gen::mov(a, R32::Eax).as_slice());
                self.exe_buf.push(Gen::binop(Binop::Add, R32::Eax, c).as_slice());
                self.exe_buf.push(Gen::mov(R32::Eax, b).as_slice());
            }
            opcodes::MULTIPLICATION => {
                let a = R32::try_from(8 + a as u8).unwrap();
                let b = R32::try_from(8 + b as u8).unwrap();
                let c = R32::try_from(8 + c as u8).unwrap();
                self.exe_buf.push(Gen::pop(R64::Rdx).as_slice());
                self.exe_buf.push(Gen::mov(a, R32::Eax).as_slice());
                self.exe_buf.push(Gen::mul_op(MulOp::Mul, c).as_slice());
                self.exe_buf.push(Gen::mov(R32::Eax, b).as_slice());
                self.exe_buf.push(Gen::push(R64::Rdx).as_slice());
            }
            opcodes::DIVISION => {
                // TODO: maybe explicitly fail on division by zero?
                let a = R32::try_from(8 + a as u8).unwrap();
                let b = R32::try_from(8 + b as u8).unwrap();
                let c = R32::try_from(8 + c as u8).unwrap();
                self.exe_buf.push(Gen::pop(R64::Rdx).as_slice());
                self.exe_buf.push(Gen::mov(a, R32::Eax).as_slice());
                self.exe_buf.push(Gen::mul_op(MulOp::Div, c).as_slice());
                self.exe_buf.push(Gen::mov(R32::Eax, b).as_slice());
                self.exe_buf.push(Gen::mov(R32::Edx, 0i32).as_slice());
                self.exe_buf.push(Gen::push(R64::Rdx).as_slice());
            }
            opcodes::NOT_AND => {
                let a = R32::try_from(8 + a as u8).unwrap();
                let b = R32::try_from(8 + b as u8).unwrap();
                let c = R32::try_from(8 + c as u8).unwrap();
                self.exe_buf.push(Gen::mov(a, R32::Eax).as_slice());
                self.exe_buf.push(Gen::binop(Binop::Xor, R32::Eax, -1i32).as_slice());
                self.exe_buf.push(Gen::binop(Binop::And, R32::Eax, c).as_slice());
                self.exe_buf.push(Gen::mov(R32::Eax, b).as_slice());
            }
            opcodes::HALT => {
                let mut t = Vec::<u8>::new();
                for &r in VOLATILE_REGS {
                    t.extend_from_slice(Gen::push(r).as_slice());
                }
                t.extend_from_slice(Gen::binop(Binop::Sub, R64::Rsp, 0x20i64).as_slice());
                t.extend_from_slice(Gen::mov(R64::Rax, halt as usize as i64).as_slice());
                t.extend_from_slice(Gen::call_indirect(R64::Rax).as_slice());
                t.extend_from_slice(Gen::binop(Binop::Add, R64::Rsp, 0x20i64).as_slice());
                for &r in VOLATILE_REGS.iter().rev() {
                    t.extend_from_slice(Gen::pop(r).as_slice());
                }

                // self.finger <- pos + 1
                t.extend_from_slice(Gen::mov(
                    Mem::base(R64::Rbx).disp(i32::try_from(memoffset::offset_of!(State, finger)).unwrap()),
                    i32::try_from(pos + 1).unwrap(),
                ).as_slice());

                // ret from 'call jump_locations[finger]' in State::run()
                t.extend_from_slice(Gen::ret().as_slice());

                self.exe_buf.push(t.as_slice());
            }
            opcodes::ALLOCATION => {
                let c = R32::try_from(8 + c as u8).unwrap();
                let b = R32::try_from(8 + b as u8).unwrap();

                // b <- call self.allocation(c)
                self.exe_buf.push(Gen::mov(b, R32::Eax).as_slice());
                for &r in VOLATILE_REGS {
                    if r != R64::Rax {
                        self.exe_buf.push(Gen::pop(r).as_slice());
                    } else {
                        self.exe_buf.push(Gen::binop(Binop::Add, R64::Rsp, 8i64).as_slice());
                    }
                }
                self.exe_buf.push(Gen::call_indirect(R64::Rax).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rax, State::allocation as usize as i64).as_slice());
                self.exe_buf.push(Gen::mov(R32::Edx, c).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rcx, R64::Rbx).as_slice());
                for &r in VOLATILE_REGS.iter().rev() {
                    if r != R64::Rax {
                        self.exe_buf.push(Gen::push(r).as_slice());
                    } else {
                        self.exe_buf.push(Gen::binop(Binop::Sub, R64::Rsp, 8i64).as_slice());
                    }
                }
            }
            opcodes::ABANDONMENT => {
                let c = R32::try_from(8 + c as u8).unwrap();
                for &r in VOLATILE_REGS {
                    self.exe_buf.push(Gen::pop(r).as_slice());
                }
                self.exe_buf.push(Gen::call_indirect(R64::Rax).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rax, State::abandonment as usize as i64).as_slice());
                self.exe_buf.push(Gen::mov(R32::Edx, c).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rcx, R64::Rbx).as_slice());
                for &r in VOLATILE_REGS.iter().rev() {
                    self.exe_buf.push(Gen::push(r).as_slice());
                }
            }
            opcodes::OUTPUT => {
                let c = R32::try_from(8 + c as u8).unwrap();
                for &r in VOLATILE_REGS {
                    self.exe_buf.push(Gen::pop(r).as_slice());
                }
                self.exe_buf.push(Gen::call_indirect(R64::Rax).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rax, output as usize as i64).as_slice());
                self.exe_buf.push(Gen::mov(R32::Ecx, c).as_slice());
                for &r in VOLATILE_REGS.iter().rev() {
                    self.exe_buf.push(Gen::push(r).as_slice());
                }
            }
            opcodes::INPUT => {
                let c = R32::try_from(8 + c as u8).unwrap();
                self.exe_buf.push(Gen::mov(c, R32::Eax).as_slice());
                for &r in VOLATILE_REGS {
                    if r != R64::Rax {
                        self.exe_buf.push(Gen::pop(r).as_slice());
                    } else {
                        self.exe_buf.push(Gen::binop(Binop::Add, R64::Rsp, 8i64).as_slice());
                    }
                }
                self.exe_buf.push(Gen::call_indirect(R64::Rax).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rax, input as usize as i64).as_slice());
                for &r in VOLATILE_REGS.iter().rev() {
                    if r != R64::Rax {
                        self.exe_buf.push(Gen::push(r).as_slice());
                    } else {
                        self.exe_buf.push(Gen::binop(Binop::Sub, R64::Rsp, 8i64).as_slice());
                    }
                }
            }
            opcodes::LOAD_PROGRAM => {
                // TODO: assert regs[b] == 0
                // TODO: assert regs[c] < jump_locations.len()
                let c = R64::try_from(8 + c as u8).unwrap();
                let b = R64::try_from(8 + b as u8).unwrap();

                self.exe_buf.push(Gen::jump_indirect(Mem::base(R64::Rsi).index_scale(c, 8)).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rax, c).as_slice());

                let no_fail = self.exe_buf.cur_pos();
                self.exe_buf.push(fail_code("LOAD_PROGRAM: c >= arrays[0].len()\0").as_slice());
                self.exe_buf.push(Gen::jump_cond(Cond::B,
                    i32::try_from(no_fail as usize - self.exe_buf.cur_pos() as usize).unwrap(),
                ).as_slice());
                self.exe_buf.push(Gen::binop(Binop::Cmp, c, R64::Rdi).as_slice());

                let no_switch_code = self.exe_buf.cur_pos();

                // rsi <- jump_locations
                self.exe_buf.push(Gen::mov(
                    R64::Rsi,
                    Mem::base(R64::Rbx).disp(i32::try_from(
                        memoffset::offset_of!(State, jump_locations) + VEC_PTR_OFFSET).unwrap()),
                ).as_slice());
                // rdi <- jump_locations.len
                self.exe_buf.push(Gen::mov(
                    R64::Rdi,
                    Mem::base(R64::Rbx).disp(i32::try_from(
                        memoffset::offset_of!(State, jump_locations) + VEC_LEN_OFFSET).unwrap()),
                ).as_slice());

                // call self.swith_code(b)
                for &r in VOLATILE_REGS {
                    self.exe_buf.push(Gen::pop(r).as_slice());
                }
                self.exe_buf.push(Gen::call_indirect(R64::Rax).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rax, State::switch_code as usize as i64).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rdx, b).as_slice());
                self.exe_buf.push(Gen::mov(R64::Rcx, R64::Rbx).as_slice());
                for &r in VOLATILE_REGS.iter().rev() {
                    self.exe_buf.push(Gen::push(r).as_slice());
                }

                // if b != 0 goto no_switch_code
                self.exe_buf.push(Gen::jump_cond(Cond::E,
                    i32::try_from(no_switch_code as usize - self.exe_buf.cur_pos() as usize).unwrap(),
                ).as_slice());
                self.exe_buf.push(Gen::binop(Binop::Cmp, b, 0i64).as_slice());
            }
            _ => panic!("op: {}", op),
        }
        self.stats.ops[op as usize].code_len += end_ptr as usize - self.exe_buf.cur_pos() as usize;
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
        xs.push(elem.clone());
        let ptr = &xs as *const _ as *const u8;
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
            code.push(opcodes::ORTHOGRAPHY << 28 | 3 << 25 | c as u32);
            code.push(opcodes::OUTPUT << 28 | 0o003);
        }
        code.push(opcodes::HALT << 28);
        let mut s = State::new(code);
        s.run();
    }
}

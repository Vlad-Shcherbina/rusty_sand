use std::convert::TryFrom;
use crate::exe_buf::ExeBuf;
use crate::amd64_gen::{Gen, R32, R64, Binop, Mem, Cond, MulOp};
use crate::interp::opcodes;

#[repr(C)]
pub struct State {
    pub finger: u32,
    pub regs: [u32; 8],
    exe_buf: ExeBuf,
    jump_locations: Vec<*const u8>,
    arrays: Vec<Vec<u32>>,
    free: Vec<u32>,
}

impl State {
    pub fn new(prog: Vec<u32>) -> State {
        let jt = jit_trampoline as usize as *const u8;
        State {
            finger: 0,
            regs: [0; 8],
            exe_buf: ExeBuf::reserve(1 << 30),
            jump_locations: vec![jt; prog.len()],
            arrays: vec![prog],
            free: vec![],
        }
    }
}

#[naked]
unsafe fn jit_trampoline() {
    asm!("
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
    println!("halt");
}

extern "win64" fn fail(s: *const std::os::raw::c_char) {
    let s = unsafe { std::ffi::CStr::from_ptr(s) };
    println!("fail: {}", s.to_str().unwrap());
    unsafe {
        asm!("int3");
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

impl State {
    fn compile_insn(&mut self, pos: usize) {
        let insn = self.arrays[0][pos];
        let op = insn >> 28;
        if op == opcodes::ORTHOGRAPHY {
            let a = ((insn >> 25) & 7) as usize;
            let imm = insn & ((1 << 25) - 1);
            self.exe_buf.push(Gen::mov(R32::try_from(8 + a as u8).unwrap(), imm as i32).as_slice());
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
            opcodes::ADDITION => {
                let a = R32::try_from(8 + a as u8).unwrap();
                let b = R32::try_from(8 + b as u8).unwrap();
                let c = R32::try_from(8 + c as u8).unwrap();
                self.exe_buf.push(Gen::binop(Binop::Add, a, c).as_slice());
                self.exe_buf.push(Gen::mov(a, b).as_slice());
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
                self.exe_buf.push(Gen::binop(Binop::Xor, a, -1i32).as_slice());
                self.exe_buf.push(Gen::binop(Binop::And, a, c).as_slice());
                self.exe_buf.push(Gen::mov(a, b).as_slice());
            }
            opcodes::HALT => {
                let mut t = Vec::<u8>::new();
                let volatile_regs = [R64::Rax, R64::Rcx, R64::Rdx, R64::R8, R64::R9, R64::R10, R64::R11];
                for &r in &volatile_regs {
                    t.extend_from_slice(Gen::push(r).as_slice());
                }
                t.extend_from_slice(Gen::binop(Binop::Sub, R64::Rsp, 0x20i64).as_slice());
                t.extend_from_slice(Gen::mov(R64::Rax, halt as usize as i64).as_slice());
                t.extend_from_slice(Gen::call_indirect(R64::Rax).as_slice());
                t.extend_from_slice(Gen::binop(Binop::Add, R64::Rsp, 0x20i64).as_slice());
                for &r in volatile_regs.iter().rev() {
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

                let no_fail = self.exe_buf.cur_pos();
                self.exe_buf.push(fail_code("LOAD_PROGRAM: b != 0\0").as_slice());
                self.exe_buf.push(Gen::jump_cond(Cond::E,
                    i32::try_from(no_fail as usize - self.exe_buf.cur_pos() as usize).unwrap(),
                ).as_slice());
                self.exe_buf.push(Gen::binop(Binop::Cmp, b, 0i64).as_slice());
            }
            _ => todo!("op: {}", op),
        }
    }

    extern "win64" fn compile(&mut self, finger: u32) {
        let finger = finger as usize;
        let mut end = finger;
        assert_eq!(self.jump_locations[end], jit_trampoline as *const u8);
        while is_fallthrough(self.arrays[0][end]) {
            end += 1;
        }
        println!("State::compile({}..={})", finger, end);
        for i in (finger..end + 1).rev() {
            self.compile_insn(i);
            self.jump_locations[finger as usize] = self.exe_buf.cur_pos();
        }
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
            asm!("
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
            : "memory", "cc"
            : "intel");
        }
        println!("done");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}

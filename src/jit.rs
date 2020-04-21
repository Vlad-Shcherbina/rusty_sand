use std::convert::TryFrom;
use crate::exe_buf::ExeBuf;
use crate::amd64_gen::{Gen, R32, R64, Binop, Mem};
use crate::interp::opcodes;

#[repr(C)]
pub struct State {
    finger: u32,
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
        match op {
            opcodes::HALT => { // halt
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
}

use std::convert::TryFrom;
use std::io::{Read, Write};

#[derive(Default, Debug)]
pub struct Stats {
    long_jumps: usize,
    ops: [usize; 16],
    num_execute_modified: usize,
    num_modify_executed: usize,
    pub word_states: Vec<WordState>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WordState {
    Initial,
    Modified,
    Executed,
}

pub struct Machine {
    regs: [u32; 8],
    finger: u32,
    arrays: Vec<Vec<u32>>,
    free: Vec<u32>,
    pub stats: Stats,
}

pub mod opcodes {
    pub const CMOVE: u32 = 0;
    pub const ARRAY_INDEX: u32 = 1;
    pub const ARRAY_AMENDMENT: u32 = 2;
    pub const ADDITION: u32 = 3;
    pub const MULTIPLICATION: u32 = 4;
    pub const DIVISION: u32 = 5;
    pub const NOT_AND: u32 = 6;
    pub const HALT: u32 = 7;
    pub const ALLOCATION: u32 = 8;
    pub const ABANDONMENT: u32 = 9;
    pub const OUTPUT: u32 = 10;
    pub const INPUT: u32 = 11;
    pub const LOAD_PROGRAM: u32 = 12;
    pub const ORTHOGRAPHY: u32 = 13;
}

pub fn insn_to_string(insn: u32) -> String {
    let op = insn >> 28;
    if op == opcodes::ORTHOGRAPHY {
        return format!("r{} = {}", ((insn >> 25) & 7), insn & ((1 << 25) - 1))
    }
    let a = ((insn >> 6) & 7) as usize;
    let b = ((insn >> 3) & 7) as usize;
    let c = (insn & 7) as usize;
    match op {
        opcodes::CMOVE => format!("if r{} != 0: r{} = r{}", c, a, b),
        opcodes::ARRAY_INDEX => format!("r{} = arrays[r{}][r{}]", a, b, c),
        opcodes::ARRAY_AMENDMENT => format!("arrays[r{}][r{}] = r{}", a, b, c),
        opcodes::ADDITION => format!("r{} = r{} + r{}", a, b, c),
        opcodes::MULTIPLICATION => format!("r{} = r{} * r{}", a, b, c),
        opcodes::DIVISION => format!("r{} = r{} / r{}", a, b, c),
        opcodes::NOT_AND => format!("r{} = ~(r{} & r{})", a, b, c),
        opcodes::OUTPUT => format!("OUTPUT r{}", c),
        opcodes::HALT => "HALT".to_string(),
        opcodes::ALLOCATION => format!("r{} = ALLOCATION r{}", b, c),
        opcodes::ABANDONMENT => format!("ABANDOMENT r{}", c),
        opcodes::LOAD_PROGRAM => format!("LOAD_PROGRAM r{} r{}", b, c),
        _ => panic!("{}", op),
    }
}

impl Machine {
    pub fn new(prog: Vec<u32>) -> Self {
        Self {
            stats: Stats {
                word_states: vec![WordState::Initial; prog.len()],
                ..Stats::default()
            },
            regs: [0; 8],
            arrays: vec![prog],
            finger: 0,
            free: vec![],
        }
    }

    pub fn run(&mut self) {
        loop {
            if self.stats.word_states[self.finger as usize] == WordState::Modified {
                self.stats.num_execute_modified += 1;
            }
            self.stats.word_states[self.finger as usize] = WordState::Executed;
            let cur = self.arrays[0][self.finger as usize];
            self.finger += 1;
            let op = cur >> 28;
            self.stats.ops[op as usize] += 1;
            let regs = &mut self.regs;

            if op == opcodes::ORTHOGRAPHY {
                let a = ((cur >> 25) & 7) as usize;
                regs[a] = cur & ((1 << 25) - 1);
                continue;
            }

            let a = ((cur >> 6) & 7) as usize;
            let b = ((cur >> 3) & 7) as usize;
            let c = (cur & 7) as usize;
            match op {
                opcodes::CMOVE =>
                    if regs[c] != 0 { regs[a] = regs[b]; }
                opcodes::ARRAY_INDEX =>
                    regs[a] = self.arrays[regs[b] as usize][regs[c] as usize],
                opcodes::ARRAY_AMENDMENT => {
                    if regs[a] == 0 {
                        if self.stats.word_states[regs[b] as usize] == WordState::Executed {
                            self.stats.num_modify_executed += 1;
                        }
                        self.stats.word_states[regs[b] as usize] = WordState::Modified;
                    }
                    self.arrays[regs[a] as usize][regs[b] as usize] = regs[c];
                }
                opcodes::ADDITION => regs[a] = regs[b].wrapping_add(regs[c]),
                opcodes::MULTIPLICATION => regs[a] = regs[b].wrapping_mul(regs[c]),
                opcodes::DIVISION => regs[a] = regs[b] / regs[c],
                opcodes::NOT_AND => regs[a] = !(regs[b] & regs[c]),
                opcodes::HALT => break,
                opcodes::ALLOCATION => {
                    let arr = vec![0; regs[c] as usize];
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
                    regs[b] = idx;
                }
                opcodes::ABANDONMENT => {
                    self.arrays[regs[c] as usize] = vec![];
                    self.free.push(regs[c]);
                }
                opcodes::OUTPUT => {
                    print!("{}", u8::try_from(regs[c]).unwrap() as char);
                    std::io::stdout().flush().unwrap();
                }
                opcodes::INPUT => {
                    let mut ch = 0u8;
                    match std::io::stdin().read_exact(std::slice::from_mut(&mut ch)) {
                        Ok(()) => regs[c] = ch as u32,
                        Err(e) => if e.kind() == std::io::ErrorKind::UnexpectedEof {
                            regs[c] = !0;
                        } else {
                            panic!("{}", e);
                        }
                    }
                }
                opcodes::LOAD_PROGRAM => {
                    if regs[b] != 0 {
                        self.arrays[0] = self.arrays[regs[b] as usize].clone();
                        self.stats.word_states = vec![WordState::Initial; self.arrays[0].len()];
                        self.stats.long_jumps += 1;
                    }
                    self.finger = regs[c];
                }
                opcodes::ORTHOGRAPHY => unreachable!(),
                _ => panic!("{}", op),
            }
        }
    }
}

use std::convert::TryFrom;
use std::io::{Read, Write};

pub struct Machine {
    regs: [u32; 8],
    finger: u32,
    arrays: Vec<Vec<u32>>,
    free: Vec<u32>,
}

impl Machine {
    pub fn new(prog: &[u32]) -> Self {
        Self {
            regs: [0; 8],
            finger: 0,
            arrays: vec![prog.to_vec()],
            free: vec![],
        }
    }

    pub fn run(&mut self) {
        loop {
            let cur = self.arrays[0][self.finger as usize];
            self.finger += 1;
            let op = cur >> 28;
            let regs = &mut self.regs;
            let a = ((cur >> 6) & 7) as usize;
            let b = ((cur >> 3) & 7) as usize;
            let c = (cur & 7) as usize;
            match op {
                0 => if regs[c] != 0 { regs[a] = regs[b]; }
                1 => regs[a] = self.arrays[regs[b] as usize][regs[c] as usize],
                2 => self.arrays[regs[a] as usize][regs[b] as usize] = regs[c],
                3 => regs[a] = regs[b].wrapping_add(regs[c]),
                4 => regs[a] = regs[b].wrapping_mul(regs[c]),
                5 => regs[a] = regs[b] / regs[c],
                6 => regs[a] = !(regs[b] & regs[c]),
                7 => break,
                8 => {
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
                9 => {
                    self.arrays[regs[c] as usize] = vec![];
                    self.free.push(regs[c]);
                }
                10 => {
                    print!("{}", u8::try_from(regs[c]).unwrap() as char);
                    std::io::stdout().flush().unwrap();
                }
                11 => {
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
                12 => {
                    if regs[b] != 0 {
                        self.arrays[0] = self.arrays[regs[b] as usize].clone();
                    }
                    self.finger = regs[c];
                }
                13 => {
                    let a = ((cur >> 25) & 7) as usize;
                    regs[a] = cur & ((1 << 25) - 1);
                }
                _ => panic!("{}", op),
            }
        }
    }
}

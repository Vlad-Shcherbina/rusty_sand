#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::convert::TryInto;
use rusty_sand::interp;
use rusty_sand::jit;

fn main() {
    let mut use_jit = false;
    let mut show_stats = false;
    let mut prog: Option<String> = None;
    for arg in std::env::args().skip(1) {
        if arg == "--jit" {
            use_jit = true;
        } else if  arg == "--stats" {
            show_stats = true;
        } else {
            assert!(prog.is_none());
            prog = Some(arg);
        }
    }
    if prog.is_none() {
        println!("Usage:");
        println!("    main [--jit] [--stats] <program.um>");
        std::process::exit(1);
    }

    let data = std::fs::read(prog.unwrap()).unwrap();
    let prog: Vec<u32> = data.chunks(4).map(
        |chunk| u32::from_be_bytes(chunk.try_into().unwrap()))
    .collect();

    if use_jit {
        let mut m = jit::State::new(prog);
        let start = std::time::Instant::now();
        m.run();
        if show_stats {
            eprintln!("it took {:.3} s", start.elapsed().as_secs_f64());
            eprintln!("compilation took {:.3} s", m.stats.compile_time);
            let mut total_cnt = 0;
            let mut total_code_len = 0;
            for (s, name) in m.stats.ops.iter().zip(&interp::OPCODE_NAMES) {
                eprintln!("{:>10} {:>10}  {}", s.cnt, s.code_len, name);
                total_cnt += s.cnt;
                total_code_len += s.code_len;
            }
            eprintln!("{:>10} {:>10}  {}", total_cnt, total_code_len, "total");
        }
    } else {
        let mut m = interp::Machine::new(prog);
        let start = std::time::Instant::now();
        m.run();
        if show_stats {
            eprintln!("it took {:.3} s", start.elapsed().as_secs_f64());
            for (cnt, name) in m.stats.ops.iter().zip(&interp::OPCODE_NAMES) {
                eprintln!("{:>12}  {}", cnt, name);
            }
        }
    }
}

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
            println!("it took {:.3} s", start.elapsed().as_secs_f64());
        }
    } else {
        let mut m = interp::Machine::new(prog);
        let start = std::time::Instant::now();
        m.run();
        if show_stats {
            println!("it took {:.3} s", start.elapsed().as_secs_f64());
            let mut num_executed = 0;
            let mut num_modified = 0;
            for s in &m.stats.word_states {
                match s {
                    crate::interp::WordState::Initial => {}
                    crate::interp::WordState::Executed => num_executed += 1,
                    crate::interp::WordState::Modified => num_modified += 1,
                }
            }
            dbg!(num_executed);
            dbg!(num_modified);
            m.stats.word_states.clear();
            println!("{:#?}", m.stats);
        }
    }
}

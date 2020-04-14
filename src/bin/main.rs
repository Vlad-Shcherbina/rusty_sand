use std::convert::TryInto;
use rusty_sand::interp;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let prog = match &args[1..] {
        [prog] => prog,
        _ => {
            println!("Usage:");
            println!("    {} <program.um>", args[0]);
            std::process::exit(1);
        }
    };
    let data = std::fs::read(prog).unwrap();
    let prog: Vec<u32> = data.chunks(4).map(
        |chunk| u32::from_be_bytes(chunk.try_into().unwrap()))
    .collect();

    let mut m = interp::Machine::new(&prog);
    let start = std::time::Instant::now();
    m.run();
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

A simple JIT for a virtual machine defined in http://boundvariable.org/.

### Benchmark

Interpreter:
```
cargo run -p rusty_sand --release --features=mimalloc --bin main data/sandmark.umz --stats
```

JIT:
```
cargo run -p rusty_sand --release --features=mimalloc --bin main data/sandmark.umz --stats --jit
```

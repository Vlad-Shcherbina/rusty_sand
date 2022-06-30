A generator of AMD64 instructions.

The instructions are emitted in reverse order,
because most jumps are forward jumps and its convenient to know jump target in advance.

For example,
```
buf.pop64(Reg::Ax);
buf.push64(Reg::Cx);
```
will generate
```
push   rcx
pop    rax
```

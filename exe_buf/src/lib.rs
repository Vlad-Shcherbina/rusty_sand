#[cfg(windows)] mod win;
#[cfg(windows)] pub use win::ExeBuf;

#[cfg(unix)] mod unix;
#[cfg(unix)] pub use unix::ExeBuf;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zzz() {
        let mut buf = ExeBuf::reserve(1 << 30);

        // https://defuse.ca/online-x86-assembler.htm
        // mov  eax, 31
        // add  eax, ecx
        // ret
        buf.push(b"\xB8\x1F\x00\x00\x00\x01\xC8\xC3");

        let f: extern "win64" fn(x: i32) -> i32 = unsafe {
            std::mem::transmute(buf.cur_pos())
        };
        dbg!(f(11));
        assert_eq!(f(11), 42);
    }
}

use std::fmt;

pub trait ByteSliceHex {
    fn fmt_hex(&self) -> ByteSliceHexFormatter;
}

impl<T> ByteSliceHex for T
where T: std::ops::Deref<Target=[u8]> {
    fn fmt_hex(&self) -> ByteSliceHexFormatter {
        ByteSliceHexFormatter {
            bytes: self.deref(),
        }
    }
}

pub struct ByteSliceHexFormatter<'a> {
    bytes: &'a [u8],
}

impl fmt::Display for ByteSliceHexFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[")?;
        for &b in self.bytes {
            write!(f, " {:02x}", b)?;
        }
        f.write_str(" ]")
    }
}

impl fmt::Debug for ByteSliceHexFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn byte_slice_hex() {
        assert_eq!(format!("{}", (&b"\x42"[..]).fmt_hex()), "[ 42 ]");
        assert_eq!(format!("{}", vec![0x42].fmt_hex()), "[ 42 ]");
    }
}

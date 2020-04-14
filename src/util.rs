use std::{cell::Cell, fmt};

// From https://github.com/rust-analyzer/rust-analyzer/blob/master/crates/stdx/src/lib.rs

pub trait SepBy: Sized {
    /// Returns an `impl fmt::Display`, which joins elements via a separator.
    fn sep_by(self, sep: &str) -> SepByBuilder<Self>;
}

impl<I> SepBy for I
where
    I: Iterator,
    I::Item: fmt::Display,
{
    fn sep_by(self, sep: &str) -> SepByBuilder<Self> {
        SepByBuilder::new(sep, self)
    }
}

pub struct SepByBuilder<'a, I> {
    sep: &'a str,
    prefix: &'a str,
    suffix: &'a str,
    iter: Cell<Option<I>>,
}

impl<'a, I> SepByBuilder<'a, I> {
    fn new(sep: &'a str, iter: I) -> SepByBuilder<'a, I> {
        SepByBuilder { sep, prefix: "", suffix: "", iter: Cell::new(Some(iter)) }
    }

    pub fn prefix(mut self, prefix: &'a str) -> Self {
        self.prefix = prefix;
        self
    }

    pub fn suffix(mut self, suffix: &'a str) -> Self {
        self.suffix = suffix;
        self
    }

    /// Set both suffix and prefix.
    pub fn surround_with(self, prefix: &'a str, suffix: &'a str) -> Self {
        self.prefix(prefix).suffix(suffix)
    }
}

impl<I> fmt::Display for SepByBuilder<'_, I>
where
    I: Iterator,
    I::Item: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.prefix)?;
        let mut first = true;
        for item in self.iter.take().unwrap() {
            if !first {
                f.write_str(self.sep)?;
            }
            first = false;
            fmt::Display::fmt(&item, f)?;
        }
        f.write_str(self.suffix)?;
        Ok(())
    }
}

// So it can be used in dbg!(), debug_struct(), etc.
impl<'a, I> fmt::Debug for SepByBuilder<'a, I>
where SepByBuilder<'a, I>: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

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
    fn sep_by() {
        assert_eq!(format!("{}", ["a", "b"].iter().sep_by(",")), "a,b");
        assert_eq!(format!("{}", ["a", "b"].iter().sep_by(",").surround_with("<", ">")), "<a,b>");
    }

    #[test]
    fn byte_slice_hex() {
        assert_eq!(format!("{}", (&b"\x42"[..]).fmt_hex()), "[ 42 ]");
        assert_eq!(format!("{}", vec![0x42].fmt_hex()), "[ 42 ]");
    }
}

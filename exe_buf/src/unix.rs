use std::ptr::null_mut;

// Grows backwards because it's more convenient for code generation
// (most jumps are jumps forward, and when generating the code backwards
// they are jumps to known locations).
#[derive(Debug)]
pub struct ExeBuf {
    reserved_start: *mut u8,
    start: *mut u8,
    end: *mut u8,
    // page_size_mask: usize,
}

const PAGE_SIZE: usize = 4096;

impl ExeBuf {
    pub fn reserve(mut size: usize) -> ExeBuf {
        size += PAGE_SIZE - 1;
        size &= !(PAGE_SIZE - 1);

        let mut start = null_mut();
        unsafe {
            libc::posix_memalign(&mut start, PAGE_SIZE, size);
            // TODO: error handling
            libc::mprotect(start, size, libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE);
            // TODO: error handling
        }

        let reserved_start = start as *mut u8;
        let end = unsafe { reserved_start.add(size) };
        ExeBuf {
            reserved_start,
            start: end,
            end,
        }
    }

    pub fn cur_pos(&self) -> *const u8 {
        self.start
    }

    pub fn push(&mut self, x: &[u8]) {
        assert!(self.reserved_start as usize + x.len() < self.start as usize);
        unsafe {
            self.start = self.start.sub(x.len());
            self.start.copy_from(x.as_ptr(), x.len());
        }
    }
}

// TODO: drop
// Grows backwards because it's more convenient for code generation
// (most jumps are jumps forward, and when generating the code backwards
// they are jumps to known locations).
#[derive(Debug)]
pub struct ExeBuf {
    reserved_start: *mut u8,
    start: *mut u8,
    end: *mut u8,
}

const PAGE_SIZE: usize = 4096;

impl ExeBuf {
    pub fn reserve(mut size: usize) -> ExeBuf {
        size += PAGE_SIZE - 1;
        size &= !(PAGE_SIZE - 1);

        let mut start = std::ptr::null_mut();
        unsafe {
            let res = libc::posix_memalign(&mut start, PAGE_SIZE, size);
            match res {
                0 => {}
                libc::EINVAL => panic!("posix_memalign: EINVAL"),
                libc::ENOMEM => panic!("posix_memalign: ENOMEM"),
                _ => panic!("posix_memalign: {}", res),
            }
            let res = libc::mprotect(start, size, libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE);
            assert_eq!(res, 0, "{}", std::io::Error::last_os_error());
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

impl Drop for ExeBuf {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.reserved_start as *mut _);
        }
    }
}

use std::convert::TryFrom;
use std::ptr::null_mut;
use winapi::um::winnt::*;

// Grows backwards because it's more convenient for code generation
// (most jumps are jumps forward, and when generating the code backwards
// they are jumps to known locations).
#[derive(Debug)]
pub struct ExeBuf {
    reserved_start: *mut u8,
    start: *mut u8,
    end: *mut u8,
    page_size_mask: usize,
}

impl ExeBuf {
    pub fn reserve(mut size: usize) -> ExeBuf {
        let mut sys_info: winapi::um::sysinfoapi::SYSTEM_INFO = unsafe { std::mem::zeroed() };
        unsafe { winapi::um::sysinfoapi::GetSystemInfo(&mut sys_info); }
        let page_size = usize::try_from(sys_info.dwPageSize).unwrap();
        assert_eq!(page_size & (page_size - 1), 0);
        let page_size_mask = !(page_size - 1);

        size += page_size - 1;
        size &= page_size_mask;

        let ptr = unsafe {
            winapi::um::memoryapi::VirtualAlloc(
                null_mut(),
                size,
                MEM_RESERVE,
                PAGE_EXECUTE_READWRITE,
            )
        };
        assert!(!ptr.is_null());
        let ptr = ptr as *mut u8;
        let end = unsafe { ptr.add(size) };
        ExeBuf {
            reserved_start: ptr,
            start: end,
            end,
            page_size_mask,
        }
    }

    pub fn cur_pos(&self) -> *const u8 {
        self.start
    }

    pub fn push(&mut self, x: &[u8]) {
        assert!(self.reserved_start as usize + x.len() < self.start as usize);
        let old_start = self.start;
        unsafe {
            self.start = self.start.sub(x.len());
            if old_start as usize & self.page_size_mask != self.start as usize & self.page_size_mask {
                let ptr = winapi::um::memoryapi::VirtualAlloc(
                    self.start as *mut _,
                    x.len(),
                    MEM_COMMIT,
                    PAGE_EXECUTE_READWRITE
                );
                assert!(!ptr.is_null());
            }
            self.start.copy_from(x.as_ptr(), x.len());
        }
    }
}

impl Drop for ExeBuf {
    fn drop(&mut self) {
        let res = unsafe {
            winapi::um::memoryapi::VirtualFree(self.reserved_start as *mut _, 0, MEM_RELEASE)
        };
        assert!(res != 0);
    }
}

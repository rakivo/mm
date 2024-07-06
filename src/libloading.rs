use std::{
    ffi::CStr,
    borrow::Cow,
};
pub use std::os::raw::{c_char as cchar, c_int as int, c_void as void};

mod dl {
    use super::*;
    extern "C" {
        pub fn dlopen(f: *const cchar, flags: int) -> *mut void;
        pub fn dlsym(h: *mut void, sym: *const cchar) -> *mut void;
        pub fn dlclose(h: *mut void) -> int;
        pub fn dlerror() -> *const cchar;
    }
}

#[macro_export]
macro_rules! cstr {
    ($s: expr) => { std::ffi::CString::new($s).unwrap() }
}

#[macro_export]
macro_rules! load_sym {
    ($lib: expr, $sym: expr, $ty: ty) => {{
        let f = load_sym($lib, $sym);
        let f: $ty = unsafe { std::mem::transmute(f) };
        f
    }}
}

pub type LResult<'a, T> = std::result::Result::<T, Cow<'a, str>>;

fn get_error() -> Option::<Cow<'static, str>> {
    let err = unsafe { dl::dlerror() };
    if err.is_null() { return None }
    Some(unsafe { CStr::from_ptr(err) }.to_string_lossy())
}

pub fn load_lib(path: &str) -> LResult::<*mut void> {
    const RTLD_NOW: int = 2;
    let cstr = cstr!(path);
    let h = unsafe { dl::dlopen(cstr.as_ptr(), RTLD_NOW) };
    if h.is_null() {
        Err(get_error().unwrap())
    } else { Ok(h) }
}

pub fn load_sym(h: *mut void, ssym: &str) -> LResult::<*const ()> {
    let cstr = cstr!(ssym);
    let sym = unsafe { dl::dlsym(h, cstr.as_ptr()) };
    if sym.is_null() {
        Err(get_error().unwrap())
    } else { Ok(sym as _) }
}

pub fn close_lib(lib: *mut void) -> LResult::<'static, ()> {
    if unsafe { dl::dlclose(lib) != 0 } {
        Err(get_error().unwrap())
    } else {
        Ok(())
    }
}

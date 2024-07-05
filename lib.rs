use std::{
    mem::transmute,
    ffi::{CString, CStr},
    os::raw::{c_char as cchar, c_int as int, c_void as void},
};

#[allow(dead_code)]
#[derive(Copy, Clone)]
struct Color {r: u8, g: u8, b: u8, a: u8}

impl Color {
    pub fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }
}

mod dl {
    use super::*;
    extern "C" {
        pub fn dlopen(f: *const cchar, flags: int) -> *mut void;
        pub fn dlsym(h: *mut void, sym: *const cchar) -> *mut void;
        pub fn dlclose(h: *mut void) -> int;
        pub fn dlerror() -> *const cchar;
    }
}

macro_rules! cstr {
    ($s: expr) => { CString::new($s).unwrap() }
}

macro_rules! load_sym {
    ($lib: expr, $sym: expr, $ty: ty) => {{
        let f = load_sym($lib, $sym);
        let f: $ty = unsafe { transmute(f) };
        f
    }}
}

fn get_error() -> Option::<String> {
    unsafe {
        let err = dl::dlerror();
        if err.is_null() { return None }
        Some(CStr::from_ptr(err).to_string_lossy().into_owned())
    }
}

fn load_lib(path: &str) -> *mut void {
    const RTLD_NOW: int = 2;
    unsafe {
        let cstr = cstr!(path);
        let h = dl::dlopen(cstr.as_ptr(), RTLD_NOW);
        if h.is_null() {
            panic!("Failed to load lib: {path}: {err}", err = get_error().unwrap());
        } h
    }
}

fn load_sym(h: *mut void, ssym: &str) -> *const () {
    let cstr = cstr!(ssym);
    unsafe {
        let sym = dl::dlsym(h, cstr.as_ptr());
        if sym.is_null() {
            panic!("Failed to load sym: {ssym}: {err}", err = get_error().unwrap());
        } sym as _
    }
}

fn close_lib(lib: *mut void) {
    if unsafe { dl::dlclose(lib) != 0 } {
        panic!("Failed to close lib: {err}", err = get_error().unwrap());
    }
}

fn main() {
    let lib = load_lib("./lib/libraylib.so");

    let init_window    = load_sym!(lib, "InitWindow", fn(int, int, *const cchar));
    let clear_back     = load_sym!(lib, "ClearBackground", fn(Color));
    let should_close   = load_sym!(lib, "WindowShouldClose", fn() -> bool);
    let set_target_fps = load_sym!(lib, "SetTargetFPS", fn(int));
    let begin_drawing  = load_sym!(lib, "BeginDrawing", fn());
    let end_drawing    = load_sym!(lib, "EndDrawing", fn());

    set_target_fps(60);

    let title = cstr!("hello");
    init_window(800, 600, title.as_ptr());

    let color = Color::new(24, 24, 24, 255);
    while !should_close() {
        begin_drawing();
            clear_back(color);
        end_drawing();
    }

    close_lib(lib);
}

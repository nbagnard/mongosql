use std::{ffi::{CString, CStr}, os::raw};

#[no_mangle]
pub extern "C" fn version() -> *const raw::c_char {
	to_extern_string("0.1.0-pre-test")
}

#[no_mangle]
pub extern "C" fn translate(sql: *const libc::c_char) -> *const raw::c_char {
	let sql = from_extern_string(sql);
	let pipeline = mongosql::translate_sql(&sql);
	to_extern_string(&pipeline)
}

fn from_extern_string(s: *const libc::c_char) -> String {
	let s = unsafe { CStr::from_ptr(s).to_bytes() };
	String::from_utf8(s.to_vec()).unwrap()
}

fn to_extern_string(s: &str) -> *const raw::c_char {
    // build a new nul-terminated string
    let c_str = CString::new(s).unwrap();

    // turn it into a pointer
    let c_str_ptr = c_str.as_ptr();

    // tell rust not to free the string when this func ends
    std::mem::forget(c_str);

    c_str_ptr
}

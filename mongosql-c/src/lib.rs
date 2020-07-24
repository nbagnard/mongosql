use std::{ffi::{CString, CStr}, os::raw};

#[no_mangle]
pub extern "C" fn translate(sql: *const libc::c_char) -> *const raw::c_char {
	let sql = unsafe { CStr::from_ptr(sql).to_bytes() };
	let sql = String::from_utf8(sql.to_vec()).unwrap();
	let pipeline = mongosql::translate_sql(&sql);
	extern_string(&pipeline)
}

fn extern_string(s: &str) -> *const raw::c_char {
    // build a new nul-terminated string
    let c_str = CString::new(s).unwrap();

    // turn it into a pointer
    let c_str_ptr = c_str.as_ptr();

    // tell rust not to free the string when this func ends
    std::mem::forget(c_str);

    c_str_ptr
}

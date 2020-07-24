use std::ffi::CStr;

#[no_mangle]
pub extern "C" fn translate(sql: *const libc::c_char) {
	let sql = unsafe { CStr::from_ptr(sql).to_bytes() };
	let sql = String::from_utf8(sql.to_vec()).unwrap();
	println!("Got SQL Query: {}", sql);
}

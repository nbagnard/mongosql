
pub fn translate_sql(sql: &str) -> String {
	let pipeline = format!(r#"[{{"$sql": {{"statement": "{}", "format": "odbc"}}}}]"#, sql);
	pipeline
}

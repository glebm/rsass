extern crate rsass;

mod unquote;

use rsass::compile_value;

pub fn check_eval(expression: &str, expected: &str) {
    assert_eq!(
        compile_value(expression.as_bytes())
            .and_then(|s| Ok(String::from_utf8(s)?))
            .unwrap(),
        expected
    )
}

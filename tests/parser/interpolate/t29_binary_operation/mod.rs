//! Tests auto-converted from "sass-spec/spec/parser/interpolate/29_binary_operation"
#[allow(unused)]
use super::rsass;
#[allow(unused)]
use rsass::precision;

/// From "sass-spec/spec/parser/interpolate/29_binary_operation/01_inline"
#[test]
fn t01_inline() {
    assert_eq!(
        rsass(
            ".result {\n  output: \"foo#{\'ba\' + \'r\'}baz\";\n  output: #{\"foo#{\'ba\' + \'r\'}baz\"};\n  output: \"[#{\"foo#{\'ba\' + \'r\'}baz\"}]\";\n  output: \"#{\"foo#{\'ba\' + \'r\'}baz\"}\";\n  output: \'#{\"foo#{\'ba\' + \'r\'}baz\"}\';\n  output: \"[\'#{\"foo#{\'ba\' + \'r\'}baz\"}\']\";\n}\n"
        )
        .unwrap(),
        ".result {\n  output: \"foobarbaz\";\n  output: foobarbaz;\n  output: \"[foobarbaz]\";\n  output: \"foobarbaz\";\n  output: \"foobarbaz\";\n  output: \"[\'foobarbaz\']\";\n}\n"
    );
}

/// From "sass-spec/spec/parser/interpolate/29_binary_operation/02_variable"
#[test]
fn t02_variable() {
    assert_eq!(
        rsass(
            "$input: \"foo#{\'ba\' + \'r\'}baz\";\n.result {\n  output: $input;\n  output: #{$input};\n  output: \"[#{$input}]\";\n  output: \"#{$input}\";\n  output: \'#{$input}\';\n  output: \"[\'#{$input}\']\";\n}\n"
        )
        .unwrap(),
        ".result {\n  output: \"foobarbaz\";\n  output: foobarbaz;\n  output: \"[foobarbaz]\";\n  output: \"foobarbaz\";\n  output: \"foobarbaz\";\n  output: \"[\'foobarbaz\']\";\n}\n"
    );
}

/// From "sass-spec/spec/parser/interpolate/29_binary_operation/03_inline_double"
#[test]
fn t03_inline_double() {
    assert_eq!(
        rsass(
            ".result {\n  output: #{#{\"foo#{\'ba\' + \'r\'}baz\"}};\n  output: #{\"[#{\"foo#{\'ba\' + \'r\'}baz\"}]\"};\n  output: #{\"#{\"foo#{\'ba\' + \'r\'}baz\"}\"};\n  output: #{\'#{\"foo#{\'ba\' + \'r\'}baz\"}\'};\n  output: #{\"[\'#{\"foo#{\'ba\' + \'r\'}baz\"}\']\"};\n}\n"
        )
        .unwrap(),
        ".result {\n  output: foobarbaz;\n  output: [foobarbaz];\n  output: foobarbaz;\n  output: foobarbaz;\n  output: [\'foobarbaz\'];\n}\n"
    );
}

/// From "sass-spec/spec/parser/interpolate/29_binary_operation/04_variable_double"
#[test]
fn t04_variable_double() {
    assert_eq!(
        rsass(
            "$input: \"foo#{\'ba\' + \'r\'}baz\";\n.result {\n  output: #{#{$input}};\n  output: #{\"[#{$input}]\"};\n  output: #{\"#{$input}\"};\n  output: #{\'#{$input}\'};\n  output: #{\"[\'#{$input}\']\"};\n}\n"
        )
        .unwrap(),
        ".result {\n  output: foobarbaz;\n  output: [foobarbaz];\n  output: foobarbaz;\n  output: foobarbaz;\n  output: [\'foobarbaz\'];\n}\n"
    );
}

/// From "sass-spec/spec/parser/interpolate/29_binary_operation/05_variable_quoted_double"
#[test]
fn t05_variable_quoted_double() {
    assert_eq!(
        rsass(
            "$input: \"foo#{\'ba\' + \'r\'}baz\";\n.result {\n  dquoted: \"#{#{$input}}\";\n  dquoted: \"#{\"[#{$input}]\"}\";\n  dquoted: \"#{\"#{$input}\"}\";\n  dquoted: \"#{\'#{$input}\'}\";\n  dquoted: \"#{\"[\'#{$input}\']\"}\";\n  squoted: \'#{#{$input}}\';\n  squoted: \'#{\"[#{$input}]\"}\';\n  squoted: \'#{\"#{$input}\"}\';\n  squoted: \'#{\'#{$input}\'}\';\n  squoted: \'#{\"[\'#{$input}\']\"}\';\n}\n"
        )
        .unwrap(),
        ".result {\n  dquoted: \"foobarbaz\";\n  dquoted: \"[foobarbaz]\";\n  dquoted: \"foobarbaz\";\n  dquoted: \"foobarbaz\";\n  dquoted: \"[\'foobarbaz\']\";\n  squoted: \"foobarbaz\";\n  squoted: \"[foobarbaz]\";\n  squoted: \"foobarbaz\";\n  squoted: \"foobarbaz\";\n  squoted: \"[\'foobarbaz\']\";\n}\n"
    );
}

// Ignoring "06_escape_interpolation", end_version is 3.5.

/// From "sass-spec/spec/parser/interpolate/29_binary_operation/06_escape_interpolation-4.0"
#[test]
fn t06_escape_interpolation_4_0() {
    assert_eq!(
        rsass(
            "$input: \"foo#{\'ba\' + \'r\'}baz\";\n.result {\n  output: \"[\\#{\"foo#{\'ba\' + \'r\'}baz\"}]\";\n  output: \"\\#{\"foo#{\'ba\' + \'r\'}baz\"}\";\n  output: \'\\#{\"foo#{\'ba\' + \'r\'}baz\"}\';\n  output: \"[\'\\#{\"foo#{\'ba\' + \'r\'}baz\"}\']\";\n}\n"
        )
        .unwrap(),
        ".result {\n  output: \"[#{\" foobarbaz \"}]\";\n  output: \"#{\" foobarbaz \"}\";\n  output: \'#{\"foobarbaz\"}\';\n  output: \"[\'#{\" foobarbaz \"}\']\";\n}\n"
    );
}

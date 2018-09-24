use css::CallArgs;
use error::Error;
use functions::SassFunction;
use num_rational::Rational;
use ordermap::OrderMap;
use std::fmt::{self, Write};
use value::{ListSeparator, Number, Operator, Quotes, Rgba, Unit};

/// A sass value.
#[derive(Clone, Debug, Eq, PartialOrd, Ord)]
pub enum Value {
    /// A special kind of escape.  Only really used for !important.
    Bang(String),
    /// An function call that was not evaluated.
    Call(String, CallArgs),
    /// A (callable?) function.
    Function(String, Option<SassFunction>),
    /// A literal string with any quoting may or may not be calculated.
    Literal(String, Quotes, bool),
    /// A comma- or space separated list of values, with or without brackets.
    List(Vec<Value>, ListSeparator, bool),
    /// A Numeric value is a rational value with a Unit (which may be
    /// Unit::None) and flags.
    ///
    /// The boolean flag is true for calculated values and false for
    /// literal values.
    Numeric(Number, Unit, bool),
    Color(Rgba, Option<String>),
    Null,
    True,
    False,
    /// A binary operation, two operands and an operator.
    /// The booleans represents possible whitespace.
    BinOp(Box<Value>, bool, Operator, bool, Box<Value>),
    UnaryOp(Operator, Box<Value>),
    Map(OrderMap<Value, Value>),
    /// A unicode range for font selections. U+NN, U+N?, U+NN-MM.
    /// The string is the entire value, including the "U+" tag.
    UnicodeRange(String),
}

// Implement PartialEq manually instead of deriving it, so a calculated
// value can be considered equal to the same, uncalculated, value
impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (&Value::Bang(ref s1), &Value::Bang(ref s2)) => s1 == s2,
            (&Value::Call(ref n1, ref a1), &Value::Call(ref n2, ref a2)) => {
                n1 == n2 && a1 == a2
            }
            (
                &Value::Function(ref n1, ref f1),
                &Value::Function(ref n2, ref f2),
            ) => n1 == n2 && f1 == f2,
            // The point of manual implementation: ignore the calcualted flag!
            (
                &Value::Literal(ref s1, ref q1, _),
                &Value::Literal(ref s2, ref q2, _),
            ) => s1 == s2 && q1 == q2,
            (
                &Value::List(ref v1, ref s1, ref b1),
                &Value::List(ref v2, ref s2, ref b2),
            ) => v1 == v2 && s1 == s2 && b1 == b2,
            (
                &Value::Numeric(ref n1, ref u1, _),
                &Value::Numeric(ref n2, ref u2, _),
            ) => n1 == n2 && u1 == u2,
            (
                &Value::Color(ref c1, ref n1),
                &Value::Color(ref c2, ref n2),
            ) => c1 == c2 && n1 == n2,
            (&Value::Null, &Value::Null)
            | (&Value::True, &Value::True)
            | (&Value::False, &Value::False) => true,
            (
                &Value::BinOp(ref a1, ref s11, ref o1, ref s21, ref b1),
                &Value::BinOp(ref a2, ref s12, ref o2, ref s22, ref b2),
            ) => a1 == a2 && s11 == s12 && o1 == o2 && s21 == s22 && b1 == b2,
            (
                &Value::UnaryOp(ref o1, ref v1),
                &Value::UnaryOp(ref o2, ref v2),
            ) => o1 == o2 && v1 == v2,
            (&Value::Map(ref m1), &Value::Map(ref m2)) => m1 == m2,
            (&Value::UnicodeRange(ref u1), &Value::UnicodeRange(ref u2)) => {
                u1 == u2
            }
            _ => false,
        }
    }
}

impl Value {
    pub fn scalar(v: isize) -> Self {
        Value::Numeric(Number::from_integer(v), Unit::None, false)
    }
    pub fn bool(v: bool) -> Self {
        if v {
            Value::True
        } else {
            Value::False
        }
    }
    pub fn black() -> Self {
        Value::Color(Rgba::from_rgb(0, 0, 0), Some("black".into()))
    }
    pub fn rgba(r: Rational, g: Rational, b: Rational, a: Rational) -> Self {
        Value::Color(Rgba::new(r, g, b, a), None)
    }
    pub fn hsla(h: Rational, s: Rational, l: Rational, a: Rational) -> Self {
        Value::Color(Rgba::from_hsla(h / 360, s, l, a), None)
    }

    pub fn type_name(&self) -> &'static str {
        match *self {
            Value::Color(..) => "color",
            Value::Literal(..) => "string",
            Value::Numeric(..) => "number",
            Value::List(..) => "list",
            Value::Function(..) => "function",
            Value::True | Value::False => "bool",
            Value::Null => "null",
            _ => "unknown",
        }
    }

    pub fn is_calculated(&self) -> bool {
        match *self {
            Value::Numeric(.., calculated) => calculated,
            Value::Color(_, None) => true,
            Value::Literal(.., calculated) => calculated,
            Value::True => true,
            //Value::False => true,
            _ => false,
        }
    }

    pub fn into_calculated(self) -> Self {
        match self {
            Value::Numeric(num, unit, _) => Value::Numeric(num, unit, true),
            Value::List(v, sep, bracketed) => Value::List(
                v.into_iter().map(|i| i.into_calculated()).collect(),
                sep,
                bracketed,
            ),
            other => other,
        }
    }

    /// All values other than `False` and `Null` should be considered true.
    pub fn is_true(&self) -> bool {
        match *self {
            Value::False | Value::Null => false,
            _ => true,
        }
    }

    pub fn is_null(&self) -> bool {
        match *self {
            Value::Null => true,
            Value::List(ref list, _, false) => {
                list.iter().all(|v| v.is_null())
            }
            _ => false,
        }
    }

    pub fn integer_value(&self) -> Result<isize, Error> {
        match self {
            &Value::Numeric(ref num, ..) if num.is_integer() => {
                Ok(num.to_integer())
            }
            v => Err(Error::bad_value("integer", v)),
        }
    }

    pub fn unquote(self) -> Value {
        match self {
            Value::Literal(s, Quotes::None, _) => {
                Value::Literal(s, Quotes::None, true)
            }
            Value::Literal(s, ..) => {
                let mut result = String::new();
                let mut iter = s.chars();
                while let Some(c) = iter.next() {
                    if c == '\\' {
                        match iter.next() {
                            Some('0') => result.push('\u{fffd}'),
                            Some(c) if (c >= '1' && c <= '9') => {
                                result.push((c as u8 - b'0') as char)
                            }
                            Some(c) if (c >= 'a' && c <= 'f') => {
                                result.push((c as u8 - b'a' + 10) as char)
                            }
                            Some(c) if (c >= 'A' && c <= 'F') => {
                                result.push((c as u8 - b'A' + 10) as char)
                            }
                            Some(c) => result.push(c),
                            None => result.push('\\'), // ??
                        }
                    } else {
                        result.push(c)
                    }
                }
                Value::Literal(result, Quotes::None, true)
            }
            Value::List(list, s, b) => Value::List(
                list.into_iter().map(|v| v.unquote()).collect(),
                s,
                b,
            ),
            v => v,
        }
    }
    pub fn unrequote(&self) -> Value {
        match *self {
            Value::Literal(ref s, Quotes::None, _) => {
                Value::Literal(s.clone(), Quotes::None, true)
            }
            Value::Literal(ref s, ..) => {
                let mut result = String::new();
                let mut iter = s.chars();
                while let Some(c) = iter.next() {
                    if c == '\\' {
                        match iter.next() {
                            Some('\\') => result.push_str("\\\\"),
                            Some('0') => result.push('\u{fffd}'),
                            Some(c) if (c >= '1' && c <= '9') => {
                                result.push((c as u8 - b'0') as char)
                            }
                            Some('a') | Some('A') => result.push_str("\\a"),
                            Some(c) if (c >= 'b' && c <= 'f') => {
                                result.push((c as u8 - b'a' + 10) as char)
                            }
                            Some(c) if (c >= 'B' && c <= 'F') => {
                                result.push((c as u8 - b'A' + 10) as char)
                            }
                            Some(c) => result.push(c),
                            None => result.push('\\'), // ??
                        }
                    } else {
                        result.push(c)
                    }
                }
                if result.contains('"') && !result.contains('\'') {
                    Value::Literal(result, Quotes::Single, true)
                } else {
                    Value::Literal(result, Quotes::Double, true)
                }
            }
            Value::List(ref list, ref s, ref b) => Value::List(
                list.into_iter().map(|v| v.unrequote()).collect(),
                s.clone(),
                *b,
            ),
            ref v => v.clone(),
        }
    }

    pub fn iter_items(self) -> Vec<Value> {
        match self {
            Value::List(v, _, _) => v,
            Value::Map(map) => map
                .iter()
                .map(|&(ref k, ref v)| {
                    Value::List(
                        vec![k.clone(), v.clone()],
                        ListSeparator::Space,
                        false,
                    )
                }).collect(),
            v => vec![v],
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, out: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Bang(ref s) => write!(out, "!{}", s),
            Value::Literal(ref s, ref q, _) => match *q {
                Quotes::Double => write!(
                    out,
                    "\"{}\"",
                    s.chars()
                        .flat_map(|c| match c {
                            '"' => vec!['\\', '"'],
                            c => vec![c],
                        }).collect::<String>()
                ),
                Quotes::Single => write!(
                    out,
                    "'{}'",
                    s.chars()
                        .flat_map(|c| match c {
                            '\'' => vec!['\\', '\''],
                            c => vec![c],
                        }).collect::<String>()
                ),
                Quotes::None => write!(out, "{}", s),
            },
            Value::Function(ref n, ref _f) => {
                let name = n
                    .chars()
                    .flat_map(|c| match c {
                        '"' => vec!['\\', '"'],
                        c => vec![c],
                    }).collect::<String>();
                write!(out, "get-function(\"{}\")", name)
            }
            Value::Numeric(ref num, ref unit, _) => {
                num.fmt(out)?;
                unit.fmt(out)
            }
            Value::Color(ref rgba, ref name) => {
                if let Some(ref name) = *name {
                    name.fmt(out)
                } else {
                    rgba.fmt(out)
                }
            }
            Value::List(ref v, ref sep, brackets) => {
                let t = v
                    .iter()
                    .filter(|v| !v.is_null())
                    .map(|v| {
                        let needs_paren = match *v {
                            Value::List(_, _, false) => {
                                brackets && *sep == ListSeparator::Space
                            }
                            _ => false,
                        };
                        match (out.alternate(), needs_paren) {
                            (true, true) => format!("({:#})", v),
                            (true, false) => format!("{:#}", v),
                            (false, true) => format!("({})", v),
                            (false, false) => format!("{}", v),
                        }
                    }).collect::<Vec<_>>();
                let t = if *sep == ListSeparator::Comma && t.len() == 1 {
                    format!("{},", t[0])
                } else {
                    t.join(match *sep {
                        ListSeparator::Comma => {
                            if out.alternate() {
                                ","
                            } else {
                                ", "
                            }
                        }
                        ListSeparator::Space => " ",
                    })
                };
                if brackets {
                    out.write_str("[")?;
                }
                write!(out, "{}", t)?;
                if brackets {
                    out.write_str("]")?;
                }
                Ok(())
            }
            Value::Call(ref name, ref arg) => {
                write!(out, "{}({})", name, arg)
            }
            Value::BinOp(ref a, _, Operator::Plus, _, ref b) => {
                // The plus operator is also a concat operator
                a.fmt(out)?;
                b.fmt(out)
            }
            Value::BinOp(ref a, ref s1, ref op, ref s2, ref b) => {
                a.fmt(out)?;
                if *s1 {
                    out.write_char(' ')?;
                }
                op.fmt(out)?;
                if *s2 {
                    out.write_char(' ')?;
                }
                b.fmt(out)
            }
            Value::UnaryOp(ref op, ref v) => {
                op.fmt(out)?;
                v.fmt(out)
            }
            Value::True => write!(out, "true"),
            Value::False => write!(out, "false"),
            Value::Null => Ok(()),
            Value::Map(ref map) => write!(
                out,
                "({})",
                map.iter()
                    .map(|&(ref k, ref v)| format!("{}: {}", k, v))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::UnicodeRange(ref s) => write!(out, "{}", s),
        }
    }
}

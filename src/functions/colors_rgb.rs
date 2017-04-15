use super::{Error, SassFunction, badarg, badargs};
use num_rational::Rational;
use num_traits::One;
use std::collections::BTreeMap;
use unit::Unit;
use valueexpression::Value;
use variablescope::Scope;

pub fn register(f: &mut BTreeMap<&'static str, SassFunction>) {
    def!(f, rgb(red, green, blue), |s| {
        Ok(Value::rgba(to_int(s.get("red"))?,
                       to_int(s.get("green"))?,
                       to_int(s.get("blue"))?,
                       Rational::one()))
    });
    def!(f, rgba(red, green, blue, alpha, color), |s: &Scope| {
        let a = s.get("alpha");
        let red = s.get("red");
        let red = if red.is_null() { s.get("color") } else { red };
        if let Value::Color(r, g, b, _, _) = red {
            let a = if a.is_null() { s.get("green") } else { a };
            Ok(Value::rgba(r, g, b, to_rational(a)?))
        } else {
            Ok(Value::rgba(to_int(red)?,
                           to_int(s.get("green"))?,
                           to_int(s.get("blue"))?,
                           to_rational(a)?))
        }
    });
    fn num(v: &Rational) -> Result<Value, Error> {
        Ok(Value::Numeric(*v, Unit::None, true))
    }
    def!(f, red(color), |s: &Scope| match &s.get("color") {
        &Value::Color(ref red, _, _, _, _) => num(red),
        value => Err(badarg("color", value)),
    });
    def!(f, green(color), |s: &Scope| match &s.get("color") {
        &Value::Color(_, ref green, _, _, _) => num(green),
        value => Err(badarg("color", value)),
    });
    def!(f, blue(color), |s: &Scope| match &s.get("color") {
        &Value::Color(_, _, ref blue, _, _) => num(blue),
        value => Err(badarg("color", value)),
    });
    def!(f, mix(color1, color2, weight = b"50%"), |s| {
        let color1 = s.get("color1");
        let color2 = s.get("color2");
        let weight = s.get("weight");
        if let (&Value::Color(ref r1, ref g1, ref b1, ref a1, _),
                &Value::Color(ref r2, ref g2, ref b2, ref a2, _),
                &Value::Numeric(ref w, ref wu, _)) =
            (&color1, &color2, &weight) {
            let w = if wu == &Unit::Percent {
                w / Rational::from_integer(100)
            } else {
                w.clone()
            };
            let one = Rational::one();
            let w2 = one - (one - w * a1) * a2;
            fn m(v1: &Rational, v2: &Rational, w: Rational) -> Rational {
                *v1 * w + *v2 * (Rational::one() - w)
            }
            Ok(Value::rgba(m(r1, r2, w2),
                           m(g1, g2, w2),
                           m(b1, b2, w2),
                           m(a1, a2, w)))
        } else {
            Err(badargs(&["color", "color", "number"],
                        &[&color1, &color2, &weight]))
        }
    });
    def!(f, invert(color), |s: &Scope| match &s.get("color") {
        &Value::Color(ref r, ref g, ref b, ref a, _) => {
            let ff = Rational::new(255, 1);
            Ok(Value::rgba(ff - r, ff - g, ff - b, *a))
        }
        value => Err(badarg("color", value)),
    });
}

fn to_int(v: Value) -> Result<Rational, Error> {
    match v {
        Value::Numeric(v, Unit::Percent, _) => Ok(Rational::new(255, 100) * v),
        Value::Numeric(v, _, _) => Ok(v),
        v => Err(badarg("number", &v)),
    }
}

fn to_rational(v: Value) -> Result<Rational, Error> {
    match v {
        Value::Numeric(v, _, _) => Ok(v),
        v => Err(badarg("number", &v)),
    }
}

#[cfg(test)]
mod test {
    use variablescope::test::do_evaluate;

    #[test]
    fn test_high() {
        assert_eq!("white", do_evaluate(&[], b"rgb(150%, 300, 256);"));
    }

    #[test]
    fn test_low() {
        assert_eq!("black", do_evaluate(&[], b"rgb(-3, -2%, 0);"));
    }
    #[test]
    fn test_mid() {
        assert_eq!("gray", do_evaluate(&[], b"rgb(50%, 255/2, 25% + 25);"));
    }
    #[test]
    fn test_named() {
        assert_eq!("gray", do_evaluate(&[], b"rgb(50%, 255/2, 25% + 25);"));
    }
}

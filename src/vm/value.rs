use crate::report::{Maybe, ReportKind, ReportLevel};
use name_variant::NamedVariant;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

#[derive(NamedVariant)]
enum ValueReport {
    TypeError(String),
}

impl Display for ValueReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())?;
        match self {
            ValueReport::TypeError(msg) => write!(f, ": {}", msg),
        }
    }
}

impl ReportKind for ValueReport {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }
}

#[repr(u8)]
#[derive(NamedVariant, Clone)]
pub enum Value {
    Integer(isize),
    Float(f64),
    Boolean(bool),
    String(String),
    Nada,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Boolean(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Nada => write!(f, "Nada"),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.variant_name())?;
        match self {
            Value::Integer(v) => write!(f, "{}", v)?,
            Value::Float(v) => write!(f, "{}", v)?,
            Value::Boolean(v) => write!(f, "{}", v)?,
            Value::String(v) => write!(f, "{:?}", v)?,
            Value::Nada => write!(f, "Nada")?,
        }
        write!(f, ")")
    }
}

macro_rules! binary_operand_error {
    ($operand:expr, $lhs:expr, $rhs:expr) => {
        ValueReport::TypeError(format!(
            "Invalid types for operand {}: {} and {}",
            $operand,
            $lhs.variant_name(),
            $rhs.variant_name()
        ))
        .make()
        .into()
    };
}

macro_rules! unary_operand_error {
    ($operand:expr, $val:expr) => {
        ValueReport::TypeError(format!(
            "Invalid type for operand {}: {}",
            $operand,
            $val.variant_name(),
        ))
        .make()
        .into()
    };
}

impl Value {
    pub fn add(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            (Value::Integer(a), Value::Float(b)) => Value::Float(*a as f64 + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a + *b as f64),
            (Value::String(a), Value::String(b)) => Value::String(format!("{}{}", a, b)),
            _ => return Err(binary_operand_error!("+", self, other)),
        })
    }

    pub fn sub(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
            (Value::Integer(a), Value::Float(b)) => Value::Float(*a as f64 - b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a - *b as f64),
            _ => return Err(binary_operand_error!("-", self, other)),
        })
    }

    pub fn mul(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
            (Value::Integer(a), Value::Float(b)) => Value::Float(*a as f64 * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a * *b as f64),
            (Value::String(a), Value::Integer(b)) => Value::String(a.repeat(*b as usize)),
            _ => return Err(binary_operand_error!("*", self, other)),
        })
    }

    pub fn pow(&self, other: &Self) -> Maybe<Self> {
        Ok(match (&self, &other) {
            (Value::Integer(a), Value::Integer(b)) => {
                Value::Integer((*a as f64).powi(*b as i32) as isize)
            }
            (Value::Integer(a), Value::Float(b)) => Value::Float((*a as f64).powf(*b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a.powf(*b)),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a.powi(*b as i32)),
            _ => return Err(binary_operand_error!("**", self, other)),
        })
    }

    pub fn div(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Float(*a as f64 / *b as f64),
            (Value::Integer(a), Value::Float(b)) => Value::Float(*a as f64 / b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a / *b as f64),
            _ => return Err(binary_operand_error!("/", self, other)),
        })
    }

    pub fn modulo(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(*a % *b),
            (Value::Integer(a), Value::Float(b)) => Value::Float(*a as f64 % b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a % *b as f64),
            _ => return Err(binary_operand_error!("%", self, other)),
        })
    }

    pub fn equal(&self, other: &Self) -> Maybe<Self> {
        Ok(Value::Boolean(self.eq(other)))
    }

    pub fn not_equal(&self, other: &Self) -> Maybe<Self> {
        Ok(Value::Boolean(self.ne(other)))
    }

    pub fn less(&self, other: &Self) -> Maybe<Self> {
        match self.partial_cmp(other) {
            Some(ordering) => Ok(Value::Boolean(ordering == Ordering::Less)),
            None => Err(binary_operand_error!("<", self, other)),
        }
    }

    pub fn less_equal(&self, other: &Self) -> Maybe<Self> {
        match self.partial_cmp(other) {
            Some(ordering) => Ok(Value::Boolean(matches!(
                ordering,
                Ordering::Less | Ordering::Equal
            ))),
            None => Err(binary_operand_error!("<=", self, other)),
        }
    }

    pub fn greater(&self, other: &Self) -> Maybe<Self> {
        other
            .less(self)
            .map_err(|_| binary_operand_error!(">", self, other))
    }

    pub fn greater_equal(&self, other: &Self) -> Maybe<Self> {
        other
            .less_equal(self)
            .map_err(|_| binary_operand_error!(">=", self, other))
    }

    pub fn bit_and(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(*a & *b),
            _ => return Err(binary_operand_error!("&", self, other)),
        })
    }

    pub fn bit_or(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(*a | *b),
            _ => return Err(binary_operand_error!("|", self, other)),
        })
    }

    pub fn bit_xor(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(*a ^ *b),
            _ => return Err(binary_operand_error!("^", self, other)),
        })
    }

    pub fn shift_left(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(*a << *b),
            _ => return Err(binary_operand_error!("<<", self, other)),
        })
    }

    pub fn shift_right(&self, other: &Self) -> Maybe<Self> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(*a >> *b),
            _ => return Err(binary_operand_error!(">>", self, other)),
        })
    }

    pub fn or(&self, other: &Self) -> Maybe<Self> {
        Ok(Value::Boolean(self.as_bool()? || other.as_bool()?))
    }

    pub fn and(&self, other: &Self) -> Maybe<Self> {
        Ok(Value::Boolean(self.as_bool()? && other.as_bool()?))
    }

    pub fn not(&self) -> Maybe<Self> {
        Ok(Value::Boolean(!self.as_bool()?))
    }

    pub fn negate(&self) -> Maybe<Self> {
        Ok(match self {
            Value::Integer(value) => Value::Integer(-*value),
            Value::Float(value) => Value::Float(-*value),
            _ => return Err(unary_operand_error!("-", self)),
        })
    }

    pub fn as_bool(&self) -> Maybe<bool> {
        Ok(match self {
            Value::Integer(val) => *val != 0,
            Value::Float(val) => *val != 0f64,
            Value::Boolean(val) => *val,
            Value::String(val) => !val.is_empty(),
            Value::Nada => false,
            // _ => {
            //     return Err(ValueReport::TypeError(format!(
            //         "Cannot coerce {} into boolean.",
            //         self.variant_name()
            //     ))
            //     .make()
            //     .into());
            // }
        })
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs.eq(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.eq(rhs),
            (Value::String(lhs), Value::String(rhs)) => lhs.eq(rhs),
            (_, _) => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Some(lhs.cmp(rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            (_, _) => None,
        }
    }
}

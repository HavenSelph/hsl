use crate::report::{Maybe, ReportKind, ReportLevel};
use name_variant::NamedVariant;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

#[derive(NamedVariant)]
enum ValueReport {
    TypeError(String),
    FloatError(String),
}

impl Display for ValueReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())?;
        match self {
            ValueReport::TypeError(msg) => write!(f, ": {}", msg),
            ValueReport::FloatError(msg) => write!(f, ": {}", msg),
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
#[derive(NamedVariant, PartialEq, Clone)]
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

impl Value {
    pub fn add(&self, other: &Value) -> Maybe<Value> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            (Value::Integer(a), Value::Float(b)) => Value::Float(*a as f64 + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a + *b as f64),
            (Value::String(a), Value::String(b)) => Value::String(format!("{}{}", a, b)),
            _ => {
                return Err(ValueReport::TypeError(format!(
                    "Cannot add {} with {}",
                    self.variant_name(),
                    other.variant_name()
                ))
                .make()
                .into());
            }
        })
    }

    pub fn sub(&self, other: &Value) -> Maybe<Value> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
            (Value::Integer(a), Value::Float(b)) => Value::Float(*a as f64 - b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a - *b as f64),
            _ => {
                return Err(ValueReport::TypeError(format!(
                    "Cannot subtract {} with {}",
                    self.variant_name(),
                    other.variant_name()
                ))
                .make()
                .into());
            }
        })
    }

    pub fn mul(&self, other: &Value) -> Maybe<Value> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
            (Value::Integer(a), Value::Float(b)) => Value::Float(*a as f64 * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a * *b as f64),
            (Value::String(a), Value::Integer(b)) => Value::String(a.repeat(*b as usize)),
            _ => {
                return Err(ValueReport::TypeError(format!(
                    "Cannot multiply {} with {}",
                    self.variant_name(),
                    other.variant_name()
                ))
                .make()
                .into());
            }
        })
    }

    pub fn div(&self, other: &Value) -> Maybe<Value> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Float(*a as f64 / *b as f64),
            (Value::Integer(a), Value::Float(b)) => Value::Float(*a as f64 / b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a / *b as f64),
            _ => {
                return Err(ValueReport::TypeError(format!(
                    "Cannot divide {} with {}",
                    self.variant_name(),
                    other.variant_name()
                ))
                .make()
                .into());
            }
        })
    }

    pub fn cmp(&self, other: &Value) -> Maybe<Ordering> {
        Ok(match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a.cmp(b),
            (Value::Float(a), Value::Float(b)) => {
                return a.partial_cmp(b).ok_or_else(|| {
                    ValueReport::FloatError(format!("Cannot compare {} with {}", a, b))
                        .make()
                        .into()
                });
            }
            _ => {
                return Err(ValueReport::TypeError(format!(
                    "Cannot compare {} with {}",
                    self.variant_name(),
                    other.variant_name()
                ))
                .make()
                .into());
            }
        })
    }

    pub fn equals(&self, other: &Value) -> Maybe<Value> {
        Ok(Value::Boolean(self.eq(other)))
    }

    pub fn gt(&self, other: &Value) -> Maybe<Value> {
        Ok(Value::Boolean(self.cmp(other)?.eq(&Ordering::Greater)))
    }

    pub fn lt(&self, other: &Value) -> Maybe<Value> {
        Ok(Value::Boolean(self.cmp(other)?.eq(&Ordering::Less)))
    }

    pub fn and(&self, other: &Value) -> Maybe<Value> {
        Ok(match (self, other) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(*a && *b),
            _ => {
                return Err(ValueReport::TypeError(format!(
                    "Cannot and {} with {}",
                    self.variant_name(),
                    other.variant_name()
                ))
                .make()
                .into());
            }
        })
    }

    pub fn or(&self, other: &Value) -> Maybe<Value> {
        Ok(match (self, other) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(*a || *b),
            _ => {
                return Err(ValueReport::TypeError(format!(
                    "Cannot or {} with {}",
                    self.variant_name(),
                    other.variant_name()
                ))
                .make()
                .into());
            }
        })
    }

    pub fn not(&self) -> Maybe<Value> {
        Ok(match self {
            Value::Boolean(value) => Value::Boolean(!*value),
            _ => {
                return Err(ValueReport::TypeError(format!(
                    "Cannot negate {}",
                    self.variant_name()
                ))
                .make()
                .into());
            }
        })
    }
}

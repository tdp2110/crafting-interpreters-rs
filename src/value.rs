use std::fmt;

#[derive(Copy, Clone)]
pub enum Value {
    Number(f64),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
        }
    }
}

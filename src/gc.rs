use crate::gc_values;
use crate::value;

enum GCData {
    String(String),
    #[allow(dead_code)]
    Closure(value::Closure),
}

impl GCData {
    fn as_str(&self) -> Option<&String> {
        match self {
            GCData::String(s) => Some(s),
            _ => None,
        }
    }
    fn as_closure(&self) -> Option<&value::Closure> {
        match &self {
            GCData::Closure(c) => Some(c),
            _ => None,
        }
    }
}

struct GCVal {
    #[allow(dead_code)]
    is_marked: bool,
    data: GCData,
}

impl GCVal {
    fn from(data: GCData) -> GCVal {
        GCVal {
            is_marked: false,
            data,
        }
    }
}

#[derive(Default)]
pub struct Heap {
    values: Vec<GCVal>,
}

impl Heap {
    #[allow(dead_code)]
    pub fn manage_str(&mut self, s: String) -> gc_values::GcString {
        self.values.push(GCVal::from(GCData::String(s)));
        gc_values::GcString(self.values.len() - 1)
    }

    #[allow(dead_code)]
    pub fn manage_closure(&mut self, c: value::Closure) -> gc_values::GcClosure {
        self.values.push(GCVal::from(GCData::Closure(c)));
        gc_values::GcClosure(self.values.len() - 1)
    }

    #[allow(dead_code)]
    pub fn get_str(&self, s: &gc_values::GcString) -> &String {
        self.values[s.0].data.as_str().unwrap()
    }
    #[allow(dead_code)]
    pub fn get_closure(&self, c: &gc_values::GcClosure) -> &value::Closure {
        self.values[c.0].data.as_closure().unwrap()
    }
}

use crate::value;

enum GCData {
    String(String),
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
    pub fn manage_str(&mut self, s: String) -> usize {
        self.values.push(GCVal::from(GCData::String(s)));
        self.values.len() - 1
    }

    pub fn manage_closure(&mut self, c: value::Closure) -> usize {
        self.values.push(GCVal::from(GCData::Closure(c)));
        self.values.len() - 1
    }

    pub fn get_str(&self, id: usize) -> &String {
        self.values[id].data.as_str().unwrap()
    }

    pub fn get_closure(&self, id: usize) -> &value::Closure {
        self.values[id].data.as_closure().unwrap()
    }

    pub fn mark(&mut self, id: usize) {
        self.values[id].is_marked = true;
    }

    pub fn mark_closure(&mut self, id: usize) {
        self.values[id].is_marked = true;
    }
}

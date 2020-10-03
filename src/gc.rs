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

pub struct Heap {
    bytes_allocated: usize,
    next_gc: usize,
    values: Vec<GCVal>,
}

impl Default for Heap {
    fn default() -> Heap {
        Heap {
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
            values: Default::default(),
        }
    }
}

impl Heap {
    pub fn manage_str(&mut self, s: String) -> usize {
        self.bytes_allocated += s.len();
        self.values.push(GCVal::from(GCData::String(s)));
        self.values.len() - 1
    }

    pub fn manage_closure(&mut self, c: value::Closure) -> usize {
        self.bytes_allocated += c.function.chunk.code.len();
        self.bytes_allocated += c.function.chunk.constants.len();
        self.values.push(GCVal::from(GCData::Closure(c)));
        self.values.len() - 1
    }

    pub fn get_str(&self, id: usize) -> &String {
        self.values[id].data.as_str().unwrap()
    }

    pub fn get_closure(&self, id: usize) -> &value::Closure {
        self.values[id].data.as_closure().unwrap()
    }

    pub fn unmark(&mut self) {
        for val in &mut self.values {
            val.is_marked = false;
        }
    }

    pub fn mark(&mut self, id: usize) {
        self.values[id].is_marked = true;
    }

    pub fn is_marked(&self, id: usize) -> bool {
        self.values[id].is_marked
    }

    pub fn children(&self, id: usize) -> Vec<usize> {
        let val = &self.values[id];

        if val.is_marked {
            return Vec::new();
        }

        match self.values[id].data {
            GCData::String(_) => return Vec::new(),
            GCData::Closure(_) => {
                return Vec::new();
            }
        }
    }

    pub fn sweep(&mut self) {
        self.values.retain(|val| val.is_marked)
    }

    pub fn should_collect(&self) -> bool {
        self.bytes_allocated >= self.next_gc
    }
}

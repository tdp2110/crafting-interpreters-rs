use crate::value;

use std::collections::HashMap;

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
    id_counter: usize,
    values: HashMap<usize, GCVal>,
}

impl Default for Heap {
    fn default() -> Heap {
        let next_gc = std::env::var("LOX_GC_TRIGGER_SIZE")
            .ok()
            .map(|env_str| env_str.parse::<usize>().ok())
            .flatten()
            .unwrap_or(1024 * 1024);
        Heap {
            bytes_allocated: 0,
            next_gc,
            id_counter: 0,
            values: Default::default(),
        }
    }
}

impl Heap {
    pub fn manage_str(&mut self, s: String) -> usize {
        self.bytes_allocated += s.len();
        let id = self.generate_id();
        self.values.insert(id, GCVal::from(GCData::String(s)));
        id
    }

    pub fn manage_closure(&mut self, c: value::Closure) -> usize {
        self.bytes_allocated += c.function.chunk.code.len();
        self.bytes_allocated += c.function.chunk.constants.len();
        let id = self.generate_id();
        self.values.insert(id, GCVal::from(GCData::Closure(c)));
        id
    }

    fn generate_id(&mut self) -> usize {
        loop {
            if !self.values.contains_key(&self.id_counter) {
                return self.id_counter;
            }
            self.id_counter += 1;
        }
    }

    pub fn get_str(&self, id: usize) -> &String {
        self.values.get(&id).unwrap().data.as_str().unwrap()
    }

    pub fn get_closure(&self, id: usize) -> &value::Closure {
        self.values.get(&id).unwrap().data.as_closure().unwrap()
    }

    pub fn unmark(&mut self) {
        for val in self.values.values_mut() {
            val.is_marked = false;
        }
    }

    pub fn mark(&mut self, id: usize) {
        self.values.get_mut(&id).unwrap().is_marked = true;
    }

    pub fn is_marked(&self, id: usize) -> bool {
        self.values.get(&id).unwrap().is_marked
    }

    pub fn children(&self, id: usize) -> Vec<usize> {
        let val = &self.values.get(&id).unwrap();

        if val.is_marked {
            return Vec::new();
        }

        match self.values.get(&id).unwrap().data {
            GCData::String(_) => Vec::new(),
            GCData::Closure(_) => Vec::new(),
        }
    }

    pub fn sweep(&mut self) {
        self.values.retain(|_, val| val.is_marked)
    }

    pub fn should_collect(&self) -> bool {
        self.bytes_allocated >= self.next_gc
    }
}

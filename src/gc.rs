use crate::gc_values;
use crate::value;

use std::collections::HashMap;

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
    fn as_str_mut(&mut self) -> Option<&mut String> {
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
    fn as_closure_mut(&mut self) -> Option<&mut value::Closure> {
        match self {
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

pub struct Heap {
    id_counter: gc_values::Id,
    values: HashMap<gc_values::Id, GCVal>,
}

impl Heap {
    #[allow(dead_code)]
    pub fn manage_str(&mut self, s: String) -> gc_values::GcString {
        let id = self.alloc_id();
        self.values.insert(id, GCVal::from(GCData::String(s)));
        gc_values::GcString(id)
    }

    #[allow(dead_code)]
    pub fn manage_closure(&mut self, c: value::Closure) -> gc_values::GcClosure {
        let id = self.alloc_id();
        self.values.insert(id, GCVal::from(GCData::Closure(c)));
        gc_values::GcClosure(id)
    }

    #[allow(dead_code)]
    fn alloc_id(&mut self) -> gc_values::Id {
        while self.values.contains_key(&self.id_counter) {
            self.id_counter += 1;
        }
        self.id_counter
    }

    #[allow(dead_code)]
    pub fn get_str(&self, s: gc_values::GcString) -> &String {
        self.values.get(&s.0).unwrap().data.as_str().unwrap()
    }
    #[allow(dead_code)]
    pub fn get_str_mut(&mut self, s: gc_values::GcString) -> &mut String {
        self.values
            .get_mut(&s.0)
            .unwrap()
            .data
            .as_str_mut()
            .unwrap()
    }
    #[allow(dead_code)]
    pub fn get_closure(&self, c: gc_values::GcClosure) -> &value::Closure {
        self.values.get(&c.0).unwrap().data.as_closure().unwrap()
    }
    #[allow(dead_code)]
    pub fn get_closure_mut(&mut self, c: gc_values::GcClosure) -> &mut value::Closure {
        self.values
            .get_mut(&c.0)
            .unwrap()
            .data
            .as_closure_mut()
            .unwrap()
    }
}

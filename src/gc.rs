use crate::value;

use std::collections::HashMap;
use std::iter::FromIterator;

enum GCData {
    String(String),
    Closure(value::Closure),
    Class(value::Class),
    Instance(value::Instance),
    BoundMethod(value::BoundMethod),
}

impl GCData {
    fn as_str(&self) -> Option<&String> {
        match self {
            GCData::String(s) => Some(s),
            _ => None,
        }
    }
    fn as_closure(&self) -> Option<&value::Closure> {
        match self {
            GCData::Closure(c) => Some(c),
            _ => None,
        }
    }
    fn as_bound_method(&self) -> Option<&value::BoundMethod> {
        match self {
            GCData::BoundMethod(m) => Some(m),
            _ => None,
        }
    }
    fn as_class(&self) -> Option<&value::Class> {
        match self {
            GCData::Class(c) => Some(c),
            _ => None,
        }
    }
    fn as_class_mut(&mut self) -> Option<&mut value::Class> {
        match self {
            GCData::Class(c) => Some(c),
            _ => None,
        }
    }
    fn as_instance(&self) -> Option<&value::Instance> {
        match self {
            GCData::Instance(inst) => Some(inst),
            _ => None,
        }
    }
    fn as_instance_mut(&mut self) -> Option<&mut value::Instance> {
        match self {
            GCData::Instance(inst) => Some(inst),
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

    pub fn manage_class(&mut self, c: value::Class) -> usize {
        let id = self.generate_id();
        self.bytes_allocated += c.name.len();
        self.bytes_allocated += c.methods.keys().map(|method_name| method_name.len()).len();
        self.values.insert(id, GCVal::from(GCData::Class(c)));
        id
    }

    pub fn manage_instance(&mut self, inst: value::Instance) -> usize {
        let id = self.generate_id();
        self.bytes_allocated += inst.fields.keys().map(|attr| attr.len()).sum::<usize>();
        self.values.insert(id, GCVal::from(GCData::Instance(inst)));
        id
    }

    pub fn manage_bound_method(&mut self, method: value::BoundMethod) -> usize {
        let id = self.generate_id();
        self.values
            .insert(id, GCVal::from(GCData::BoundMethod(method)));
        id
    }

    fn generate_id(&mut self) -> usize {
        self.id_counter += 1;
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

    pub fn get_bound_method(&self, id: usize) -> &value::BoundMethod {
        self.values
            .get(&id)
            .unwrap()
            .data
            .as_bound_method()
            .unwrap()
    }

    pub fn get_class(&self, id: usize) -> &value::Class {
        self.values.get(&id).unwrap().data.as_class().unwrap()
    }

    pub fn get_class_mut(&mut self, id: usize) -> &mut value::Class {
        self.values
            .get_mut(&id)
            .unwrap()
            .data
            .as_class_mut()
            .unwrap()
    }

    pub fn get_instance(&self, id: usize) -> &value::Instance {
        self.values.get(&id).unwrap().data.as_instance().unwrap()
    }

    pub fn get_instance_mut(&mut self, id: usize) -> &mut value::Instance {
        self.values
            .get_mut(&id)
            .unwrap()
            .data
            .as_instance_mut()
            .unwrap()
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
        match &self.values.get(&id).unwrap().data {
            GCData::String(_) => Vec::new(),
            GCData::Closure(closure) => self.closure_children(closure),
            GCData::Class(class) => self.class_children(class),
            GCData::Instance(instance) => self.instance_children(instance),
            GCData::BoundMethod(method) => self.bound_method_children(method),
        }
    }

    pub fn bound_method_children(&self, method: &value::BoundMethod) -> Vec<usize> {
        vec![method.instance_id, method.closure_id]
    }

    pub fn class_children(&self, class: &value::Class) -> Vec<usize> {
        Vec::from_iter(class.methods.values().copied())
    }

    pub fn instance_children(&self, instance: &value::Instance) -> Vec<usize> {
        let mut res = Vec::new();
        res.push(instance.class_id);

        for field in instance.fields.values() {
            if let Some(id) = self.extract_id(field) {
                res.push(id)
            }
        }

        res
    }

    pub fn closure_children(&self, closure: &value::Closure) -> Vec<usize> {
        let res: Vec<usize> = closure
            .upvalues
            .iter()
            .map(|upval| match &*upval.borrow() {
                value::Upvalue::Open(_) => None,
                value::Upvalue::Closed(value) => self.extract_id(&value),
            })
            .flatten()
            .collect();
        res
    }

    pub fn extract_id(&self, val: &value::Value) -> Option<usize> {
        match val {
            value::Value::String(id) => Some(*id),
            value::Value::Function(id) => Some(*id),
            _ => None,
        }
    }

    pub fn sweep(&mut self) {
        self.values.retain(|_, val| val.is_marked)
    }

    pub fn should_collect(&self) -> bool {
        self.bytes_allocated >= self.next_gc
    }
}

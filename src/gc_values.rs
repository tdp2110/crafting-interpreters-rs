pub type Id = u64;

#[derive(Copy, Clone)]
pub struct GcString(pub Id);
#[derive(Copy, Clone)]
pub struct GcClosure(pub Id);

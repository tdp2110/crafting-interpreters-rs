pub enum Source {
    Literal,
    File(String),
}

pub struct Input {
    pub source: Source,
    pub content: String,
}

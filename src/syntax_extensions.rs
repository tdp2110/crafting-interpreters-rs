#[derive(Copy, Clone)]
pub struct Extensions {
    pub lists: bool,
}

impl Default for Extensions {
    fn default() -> Extensions {
        Extensions { lists: false }
    }
}

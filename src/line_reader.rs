pub struct LineReader {
    rl: rustyline::Editor<()>,
    history_file: String,
}

impl Drop for LineReader {
    fn drop(&mut self) {
        self.rl.save_history(&self.history_file).unwrap();
    }
}

pub enum LineReadStatus {
    Line(String),
    Done,
}

impl LineReader {
    pub fn new(history_file: String) -> LineReader {
        let mut rl = rustyline::Editor::<()>::new();
        rl.load_history(&history_file).ok();
        LineReader { rl, history_file }
    }

    pub fn readline(&mut self) -> LineReadStatus {
        let res = self.rl.readline(">>> ");

        match res {
            Ok(line) => {
                self.rl.add_history_entry(line.as_str());
                LineReadStatus::Line(line)
            }
            Err(_) => LineReadStatus::Done,
        }
    }
}

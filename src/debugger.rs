use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::bytecode;
use crate::bytecode_interpreter;
use crate::line_reader;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

macro_rules! vec_of_strings {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
}

pub struct Debugger {
    interpreter: bytecode_interpreter::Interpreter,
    lines: Vec<String>,
    last_command: Option<DebugCommand>,
    interrupted: Arc<AtomicBool>,
    breakpoints: HashSet<usize>,
    command_map: HashMap<String, DebugCommand>,
    command_list: Vec<(Vec<String>, DebugCommand)>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum DebugCommand {
    Dis,
    Op,
    Step,
    Quit,
    Stack,
    Globals,
    Upvals,
    List,
    RepeatOrNil,
    Go,
    Break(usize),
    Backtrace,
    Heap,
    Help,
    Unknown,
}

enum ShouldBreak {
    True,
    False,
}

enum Verbosity {
    Verbose,
    None,
}

impl Debugger {
    pub fn new(func: bytecode::Function, input: String) -> Debugger {
        let lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();
        let mut interpreter = bytecode_interpreter::Interpreter::default();
        interpreter.prepare_interpret(func);

        let interrupted = Arc::new(AtomicBool::new(true));

        {
            let interrupted_clone = interrupted.clone();

            ctrlc::set_handler(move || {
                interrupted_clone.store(true, Ordering::Release);
            })
            .expect("Error setting Ctrl-C handler");
        }

        let command_list = vec![
            (vec_of_strings!["dis"], DebugCommand::Dis),
            (vec_of_strings!["op"], DebugCommand::Op),
            (vec_of_strings!["step", "s"], DebugCommand::Step),
            (vec_of_strings!["quit", "q"], DebugCommand::Quit),
            (vec_of_strings!["stack"], DebugCommand::Stack),
            (vec_of_strings!["globals"], DebugCommand::Globals),
            (vec_of_strings!["upvals"], DebugCommand::Upvals),
            (vec_of_strings!["list"], DebugCommand::List),
            (vec_of_strings![""], DebugCommand::RepeatOrNil),
            (vec_of_strings!["go", "g"], DebugCommand::Go),
            (vec_of_strings!["backtrace", "bt"], DebugCommand::Backtrace),
            (vec_of_strings!["heap"], DebugCommand::Heap),
            (vec_of_strings!["help", "h"], DebugCommand::Help),
        ];

        let command_map = command_list.iter().fold(HashMap::new(), |mut acc, elt| {
            let (command_strings, command) = elt;
            for command_string in command_strings {
                acc.insert(command_string.clone(), *command);
            }
            acc
        });

        Debugger {
            interpreter,
            lines,
            last_command: None,
            interrupted,
            breakpoints: Default::default(),
            command_map,
            command_list,
        }
    }

    pub fn debug(&mut self) {
        println!(
            "===================================================\n\
             Welcome to loxdb {}, the lox debugger! \n\
             Authors: {}\n\
             \n\
             Enter \"help\" (or \"h\") for list of commands.\n\
             ===================================================\n",
            VERSION, AUTHORS
        );
        let mut line_reader = line_reader::LineReader::new(".debugger-history.txt", "(loxdb) ");

        loop {
            if !self.interpreter.is_done() && !self.interrupted.load(Ordering::Acquire) {
                if self.breakpoints.contains(&self.interpreter.next_line()) {
                    self.interrupted.store(true, Ordering::Release);
                    self.breakpoints.remove(&self.interpreter.next_line());
                    println!(
                        "reached breakpoint at line {}",
                        self.interpreter.next_line()
                    );
                } else {
                    self.execute_command(DebugCommand::Step, Verbosity::None);
                    continue;
                }
            }

            let readline = line_reader.readline();

            match readline {
                line_reader::LineReadStatus::Line(line) => {
                    let line = line.trim();

                    let command = self.read_command(line);
                    if let ShouldBreak::True = self.execute_command(command, Verbosity::Verbose) {
                        break;
                    }
                    if command != DebugCommand::RepeatOrNil {
                        self.last_command = Some(command);
                    }
                }
                line_reader::LineReadStatus::Done => break,
            }
        }
    }

    fn execute_command(&mut self, command: DebugCommand, verbosity: Verbosity) -> ShouldBreak {
        match command {
            DebugCommand::Dis => {
                let func = &self.interpreter.frame().closure.function;
                let dis_output = bytecode_interpreter::disassemble_chunk(&func.chunk, &func.name);
                println!("{}", dis_output);
            }
            DebugCommand::Op => {
                let frame = self.interpreter.frame();
                println!("{:?}", frame.closure.function.chunk.code[frame.ip].0);
            }
            DebugCommand::Step => {
                if self.interpreter.is_done() {
                    println!("cannot step a completed program");
                    return ShouldBreak::True;
                }
                match self.interpreter.step() {
                    Ok(()) => {
                        if let Verbosity::Verbose = verbosity {
                            self.list()
                        }
                    }
                    Err(err) => println!(
                        "{}.\n\nTraceback:\n\n{}",
                        err,
                        self.interpreter.format_backtrace()
                    ),
                }
            }
            DebugCommand::Quit => {
                return ShouldBreak::True;
            }
            DebugCommand::Stack => {
                for val in self.interpreter.stack.iter().rev() {
                    println!("{}", self.interpreter.format_val(val));
                }
            }
            DebugCommand::Globals => {
                if self.interpreter.globals.is_empty() {
                    println!("<empty globals>");
                }
                for (name, val) in &self.interpreter.globals {
                    println!("{}: {}", name, self.interpreter.format_val(val));
                }
            }
            DebugCommand::Upvals => {
                if self.interpreter.upvalues.is_empty() {
                    println!("<empty upvals>")
                }
                for val in &self.interpreter.upvalues {
                    println!("{}", self.interpreter.format_upval(&val.borrow()));
                }
            }
            DebugCommand::List => self.list(),
            DebugCommand::Go => {
                self.interrupted.store(false, Ordering::Release);
                let line = self.interpreter.next_line();
                self.run_until_off_line(line);
            }
            DebugCommand::Break(lineno) => {
                println!("inserted breakpoint at line {}", lineno);
                self.breakpoints.insert(lineno);
            }
            DebugCommand::RepeatOrNil => {
                if let Some(last_command) = self.last_command {
                    self.execute_command(last_command, verbosity);
                }
            }
            DebugCommand::Backtrace => {
                println!("{}", self.interpreter.format_backtrace());
            }
            DebugCommand::Heap => println!("{}", self.interpreter.heap.summarize_stats()),
            DebugCommand::Help => self.print_help(),
            DebugCommand::Unknown => {}
        }
        ShouldBreak::False
    }

    fn run_until_off_line(&mut self, line: usize) {
        loop {
            if self.interpreter.is_done() || self.interpreter.next_line() != line {
                break;
            }
            self.execute_command(DebugCommand::Step, Verbosity::None);
        }
    }

    fn print_help(&self) {
        println!("Debugger commands:");
        for (command_strings, command) in &self.command_list {
            if *command != DebugCommand::RepeatOrNil {
                println!(
                    "  {:<15} -- {}",
                    command_strings.join(", "),
                    Debugger::describe_command(*command)
                );
            }
        }
    }

    fn describe_command(cmd: DebugCommand) -> String {
        match cmd {
            DebugCommand::Dis => "Disassemble for current frame.".to_string(),
            DebugCommand::Op => "Show the current opcode.".to_string(),
            DebugCommand::Step => "Step to next opcode.".to_string(),
            DebugCommand::Quit => "Quit the debugger.".to_string(),
            DebugCommand::Stack => "Show the stack.".to_string(),
            DebugCommand::Globals => "Show the globals.".to_string(),
            DebugCommand::Upvals => "Show the upvalues.".to_string(),
            DebugCommand::List => {
                "List the source and disassembly around current line/op.".to_string()
            }
            DebugCommand::RepeatOrNil => panic!(),
            DebugCommand::Go => {
                "Execute until program termination, a breakpoint is hit, or program is interrupted."
                    .to_string()
            }
            DebugCommand::Break(lineno) => format!("Set a breakpoint at line {}.", lineno),
            DebugCommand::Backtrace => "Show backtrace.".to_string(),
            DebugCommand::Heap => "Show heap statistics.".to_string(),
            DebugCommand::Help => "Show debugger commands.".to_string(),
            DebugCommand::Unknown => panic!(),
        }
    }

    fn list(&self) {
        if let Some(frame) = self.interpreter.maybe_frame() {
            let ip = frame.ip;

            if self.interpreter.is_done() {
                println!("program completed");
                return;
            }

            let maxdist = 4;

            self.lines
                .iter()
                .enumerate()
                .filter(|(idx, _)| {
                    let next_line = self.interpreter.next_line();
                    if next_line < *idx {
                        *idx - next_line < maxdist
                    } else {
                        next_line - *idx < maxdist
                    }
                })
                .for_each(|(idx, line)| {
                    let prefix = if idx + 1 == self.interpreter.next_line() {
                        "==>"
                    } else {
                        "   "
                    };
                    println!("{} {:<4} {}", prefix, idx + 1, line)
                });

            println!();

            let chunk = &self.interpreter.frame().closure.function.chunk;
            let dissed_code = bytecode_interpreter::disassemble_code(chunk);
            dissed_code
                .iter()
                .enumerate()
                .filter(|(idx, _)| {
                    if ip < *idx {
                        *idx - ip < maxdist
                    } else {
                        ip - *idx < maxdist
                    }
                })
                .for_each(|(idx, line)| {
                    let prefix = if idx == ip { "==>" } else { "   " };
                    println!("{} {}", prefix, line);
                });
        }
    }

    fn read_command(&self, input: &str) -> DebugCommand {
        match self.command_map.get(input) {
            Some(cmd) => *cmd,
            None => {
                let words: Vec<_> = input.split_whitespace().collect();
                if words.len() == 2 && words[0] == "b" {
                    if let Ok(lineno) = words[1].parse::<usize>() {
                        return DebugCommand::Break(lineno);
                    }
                }
                println!("unknown command: {}", input);
                DebugCommand::Unknown
            }
        }
    }
}

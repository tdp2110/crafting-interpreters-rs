extern crate clap;
extern crate ctrlc;

use clap::{App, Arg};

use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::io::{self, BufRead, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

mod builtins;
mod bytecode;
mod bytecode_interpreter;
mod compiler;
mod expr;
mod gc;
mod parser;
mod scanner;
mod treewalk_interpreter;
mod value;

static INPUT_STR: &str = "INPUT";
static SHOW_TOKENS_STR: &str = "tokens";
static SHOW_AST_STR: &str = "ast";
static DISASSEMBLE_STR: &str = "disassemble";
static DEBUG_STR: &str = "debug";
static TREEWALK_STR: &str = "treewalk";

macro_rules! vec_of_strings {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
}

fn run_repl() {
    let mut interpreter: treewalk_interpreter::Interpreter = Default::default();
    println!(
        "============================================\n\
         Welcome to lox! using tree-walk interpreter.\n\
         ============================================\n"
    );

    {
        let interrupt_clone = interpreter.interrupted.clone();
        ctrlc::set_handler(move || {
            interrupt_clone.store(true, Ordering::Release);
        })
        .expect("Error setting Ctrl-C handler");
    }

    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        let stdin = io::stdin();
        let nchar = stdin
            .lock()
            .read_line(&mut line)
            .expect("Could not read line");
        if nchar == 0 {
            break;
        }

        match scanner::scan_tokens(line) {
            Ok(tokens) => match parser::parse(tokens) {
                Ok(stmts) => {
                    let stmts2: Vec<expr::Stmt> = stmts
                        .iter()
                        .map(|stmt| match stmt {
                            expr::Stmt::Expr(expr) => expr::Stmt::Print(expr.clone()),
                            _ => stmt.clone(),
                        })
                        .collect();
                    match interpreter.interpret(&stmts2) {
                        Ok(()) => {}
                        Err(err) => println!("Runtime error: {}", err),
                    }
                }
                Err(err) => println!("\nParse error: {}", err),
            },
            Err(err) => {
                println!("\nScanner error: {}", err);
            }
        }
    }
}

struct Debugger {
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
    Help,
    Unknown,
}

impl Debugger {
    fn init(func: bytecode::Function, input: String) -> Debugger {
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
            (vec_of_strings!["help", "h"], DebugCommand::Help),
        ];

        let command_map = command_list.iter().fold(HashMap::new(), |mut acc, elt| {
            let (command_strings, command) = elt;
            for command_string in command_strings {
                acc.insert(command_string.clone(), command.clone());
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

    fn debug(&mut self) {
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
                    self.execute_command(DebugCommand::Step, false);
                    continue;
                }
            }

            print!("(loxdb) ");
            io::stdout().flush().unwrap();
            let mut line = String::new();
            let stdin = io::stdin();
            let nchar = stdin
                .lock()
                .read_line(&mut line)
                .expect("Could not read line");
            if nchar == 0 {
                break;
            }
            let line = line.trim();

            let command = self.read_command(&line);
            if self.execute_command(command, true) {
                break;
            }
            if command != DebugCommand::RepeatOrNil {
                self.last_command = Some(command);
            }
        }
    }

    // returns true if should break, false otherwise
    fn execute_command(&mut self, command: DebugCommand, verbose_step: bool) -> bool {
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
                    return true;
                }
                match self.interpreter.step() {
                    Ok(()) => {
                        if verbose_step {
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
                return true;
            }
            DebugCommand::Stack => {
                for val in self.interpreter.stack.iter().rev() {
                    println!("{}", self.interpreter.format_val(&val));
                }
            }
            DebugCommand::Globals => {
                if self.interpreter.globals.is_empty() {
                    println!("<empty globals>");
                }
                for (name, val) in &self.interpreter.globals {
                    println!("{}: {}", name, self.interpreter.format_val(&val));
                }
            }
            DebugCommand::Upvals => {
                if self.interpreter.upvalues.is_empty() {
                    println!("<empty upvals>")
                }
                for val in &self.interpreter.upvalues {
                    println!("{}", self.interpreter.format_upval(&*val.borrow()));
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
                    self.execute_command(last_command, verbose_step);
                }
            }
            DebugCommand::Backtrace => {
                println!("{}", self.interpreter.format_backtrace());
            }
            DebugCommand::Help => self.print_help(),
            DebugCommand::Unknown => {}
        }
        false
    }

    fn run_until_off_line(&mut self, line: usize) {
        loop {
            if self.interpreter.is_done() || self.interpreter.next_line() != line {
                break;
            }
            self.execute_command(DebugCommand::Step, false);
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
            let dissed_code = bytecode_interpreter::disassemble_code(&chunk);
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

fn main() {
    let matches = App::new("loxi")
        .version("0.1.0")
        .about("lox language interpreter")
        .author("Thomas Peters")
        .arg(
            Arg::with_name(INPUT_STR)
                .help("sets input file to use")
                .required(false)
                .index(1),
        )
        .arg(
            Arg::with_name(SHOW_TOKENS_STR)
                .long("--show-tokens")
                .takes_value(false)
                .help("show the token stream"),
        )
        .arg(
            Arg::with_name(SHOW_AST_STR)
                .long("--show-ast")
                .takes_value(false)
                .help("show the AST"),
        )
        .arg(
            Arg::with_name(DISASSEMBLE_STR)
                .long("--disassemble")
                .takes_value(false)
                .help("show the bytecode"),
        )
        .arg(
            Arg::with_name(DEBUG_STR)
                .long("--debug")
                .takes_value(false)
                .help("run in the debugger"),
        )
        .arg(
            Arg::with_name(TREEWALK_STR)
                .long("--treewalk")
                .takes_value(false)
                .help("run the tree-walk interpreter instead of the bytecode interpreter"),
        )
        .get_matches();

    if let Some(input_file) = matches.value_of(INPUT_STR) {
        let maybe_input = fs::read_to_string(input_file);

        match maybe_input {
            Ok(input) => {
                if matches.is_present(SHOW_TOKENS_STR)
                    || matches.is_present(SHOW_AST_STR)
                    || matches.is_present(TREEWALK_STR)
                {
                    match scanner::scan_tokens(input) {
                        Ok(tokens) => {
                            if matches.is_present(SHOW_TOKENS_STR) {
                                println!("tokens: {:#?}", tokens);
                                std::process::exit(0);
                            }

                            let stmts_maybe = parser::parse(tokens);

                            match stmts_maybe {
                                Ok(stmts) => {
                                    if matches.is_present(SHOW_AST_STR) {
                                        println!("AST: {:#?}", stmts);
                                        std::process::exit(0);
                                    }

                                    let interpret_result = treewalk_interpreter::interpret(&stmts);

                                    match interpret_result {
                                        Ok(_) => {
                                            std::process::exit(0);
                                        }
                                        Err(err) => {
                                            println!("Treewalk Interpreter Error: {}", err);
                                            std::process::exit(-1);
                                        }
                                    }
                                }
                                Err(err) => {
                                    println!("parse error: {}", err);
                                    std::process::exit(-1)
                                }
                            }
                        }
                        Err(err) => {
                            println!("lexical error: {}", err);
                            std::process::exit(-1);
                        }
                    }
                }

                let func_or_err = compiler::Compiler::compile(input.clone());

                match func_or_err {
                    Ok(func) => {
                        if matches.is_present(DISASSEMBLE_STR) {
                            println!(
                                "{}",
                                bytecode_interpreter::disassemble_chunk(&func.chunk, input_file)
                            );
                            std::process::exit(0);
                        }
                        if matches.is_present(DEBUG_STR) {
                            Debugger::init(func, input).debug();
                            std::process::exit(0);
                        }
                        let res = bytecode_interpreter::Interpreter::default().interpret(func);
                        match res {
                            Ok(()) => {
                                std::process::exit(0);
                            }
                            Err(err) => {
                                println!("{:?}", err);
                                std::process::exit(1);
                            }
                        }
                    }
                    Err(err) => {
                        println!("{}", err);
                        std::process::exit(1);
                    }
                }
            }
            Err(err) => {
                println!("Error reading {}: {}", input_file, err);
                std::process::exit(-1);
            }
        }
    } else {
        run_repl();
    }
}

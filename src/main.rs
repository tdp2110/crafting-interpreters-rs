extern crate clap;

use clap::{App, Arg};

use std::fs;
use std::io::{self, BufRead, Write};

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

struct Debugger {
    interpreter: bytecode_interpreter::Interpreter,
    lines: Vec<String>,
    last_command: Option<DebugCommand>,
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
    Unknown,
}

impl Debugger {
    fn init(func: bytecode::Function, input: String) -> Debugger {
        let lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();
        let mut interpreter = bytecode_interpreter::Interpreter::default();
        interpreter.prepare_interpret(func);
        Debugger {
            interpreter,
            lines,
            last_command: None,
        }
    }

    fn debug(&mut self) {
        loop {
            if self.interpreter.is_done() {
                println!("(loxdb) done");
            }
            print!("(loxdb) ");
            io::stdout().flush().unwrap();
            let mut line = String::new();
            let stdin = io::stdin();
            stdin
                .lock()
                .read_line(&mut line)
                .expect("Could not read line");
            let line = line.trim();

            let command = Debugger::read_command(&line);
            if self.execute_command(command) {
                break;
            }
            if command != DebugCommand::RepeatOrNil {
                self.last_command = Some(command);
            }
        }
    }

    // returns true if should break, false otherwise
    fn execute_command(&mut self, command: DebugCommand) -> bool {
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
            DebugCommand::Step => match self.interpreter.step() {
                Ok(()) => self.list(),
                Err(err) => println!("{}", err),
            },
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
            DebugCommand::RepeatOrNil => {
                if let Some(last_command) = self.last_command {
                    self.execute_command(last_command);
                }
            }
            DebugCommand::Unknown => println!("\nunknown command"),
        }
        false
    }

    fn list(&self) {
        let maxdist = 4;

        self.lines
            .iter()
            .enumerate()
            .filter(|(idx, _)| {
                if self.interpreter.line < *idx {
                    *idx - self.interpreter.line < maxdist
                } else {
                    self.interpreter.line - *idx < maxdist
                }
            })
            .for_each(|(idx, line)| {
                let prefix = if idx == self.interpreter.line {
                    "==>"
                } else {
                    "   "
                };
                println!("{} {:<4} {}", prefix, idx, line)
            });

        println!();

        let ip = self.interpreter.frame().ip;
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

    fn read_command(input: &str) -> DebugCommand {
        match input {
            "dis" => DebugCommand::Dis,
            "op" => DebugCommand::Op,
            "step" | "s" => DebugCommand::Step,
            "quit" | "q" => DebugCommand::Quit,
            "stack" => DebugCommand::Stack,
            "globals" => DebugCommand::Globals,
            "upvals" => DebugCommand::Upvals,
            "list" => DebugCommand::List,
            "" => DebugCommand::RepeatOrNil,
            _ => DebugCommand::Unknown,
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
                .required(true)
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
                    match scanner::scan_tokens(input.clone()) {
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
                                        Ok(output) => {
                                            println!("{}", output);
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
                            bytecode_interpreter::disassemble_chunk(&func.chunk, input_file);
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
    }
}

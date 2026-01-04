use std::io::{Write, stdin, stdout};

use crate::ast::Node;
use crate::ast::parser::Parser;
use crate::dprintln;
use crate::files::push_source;
use crate::report::{ReportChannel, ReportLevel, UnwrapReport};
use crate::vm::{Compiler, VM};

const REPL_VERSION: &str = "0.0.1";

#[derive(PartialOrd, PartialEq)]
pub enum ParseResult {
    Ok,
    Incomplete,
    Error,
}

pub struct Repl<'vm, 'channel> {
    vm: &'vm mut VM,
    report_channel: &'channel ReportChannel,
    input_counter: usize,
}

impl<'v, 'c> Repl<'v, 'c> {
    pub fn new(vm: &'v mut VM, report_channel: &'c ReportChannel) -> Self {
        Self {
            vm,
            report_channel,
            input_counter: 1,
        }
    }

    fn try_parse(&self, cache_source: &'static str) -> Result<Box<Node>, ParseResult> {
        let ast = Parser::new(cache_source, self.report_channel.get_sender())
            .unwrap_reported()
            .parse();

        let mut parse_result = ParseResult::Ok;
        for report in self.report_channel.receiver.try_iter() {
            let this_result: ParseResult = if report.level < ReportLevel::Error {
                ParseResult::Ok
            } else if report.incomplete {
                ParseResult::Incomplete
            } else {
                ReportChannel::display(*report.clone());
                ParseResult::Error
            };
            if this_result > parse_result {
                parse_result = this_result;
            }
        }

        match parse_result {
            ParseResult::Ok => Ok(ast),
            _ => Err(parse_result),
        }
    }

    fn run(&mut self) -> Result<(), ()> {
        let mut input = String::new();
        let mut last = String::new();
        let name = Box::leak(format!("<{:0>3}>", self.input_counter).into_boxed_str());
        self.input_counter += 1;

        let ast = loop {
            let mut temp = String::new();
            print!("{}", if input.is_empty() { ">>> " } else { "... " });
            stdout().flush().expect("Failed to flush stdout");
            stdin().read_line(&mut temp).expect("Failed to read line");
            if temp.trim().is_empty() && last.trim().is_empty() {
                return Ok(());
            };
            last = temp.clone();
            input.push_str(&temp);
            push_source(name, input.clone());
            match self.try_parse(name) {
                Ok(ast) => break ast,
                Err(ParseResult::Incomplete) => continue,
                _ => return Err(()),
            }
        };
        dprintln!("{ast}");

        let mut chunk = {
            let mut compiler = Compiler::new();
            compiler.compile_program(&ast);
            compiler.chunk
        };

        let val = self
            .vm
            .run(&mut chunk)
            .map_err(|err| {
                ReportChannel::display(err.finish());
            })
            .unwrap_or(crate::vm::Value::Nada);
        match val {
            crate::vm::Value::Nada => {}
            _ => {
                // todo: inject previous evaluation into scope
                println!("{val:?}");
            }
        }
        Ok(())
    }

    pub fn start_loop(&mut self) {
        println!(
            "HSL {} | REPL {REPL_VERSION}\nPress Ctrl-C to exit",
            env!("CARGO_PKG_VERSION")
        );
        loop {
            self.run().ok(); // Discarding error
        }
    }
}

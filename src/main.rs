#![warn(clippy::complexity)]
#![allow(clippy::upper_case_acronyms)]

use crate::ast::parser::Parser;
use crate::repl::Repl;
use crate::report::{Maybe, ReportChannel};
use crate::vm::{Compiler, VM};
use args::ARGS;

mod ast;
mod vm;

mod args;
mod debug;
mod files;
mod repl;
mod report;

fn run_file(
    filename: &'static str,
    vm: &mut VM,
    report_channel: &mut ReportChannel,
) -> Maybe<vm::Value> {
    let ast = Parser::new(filename, report_channel.get_sender())?.parse();
    dprintln!("{ast}");

    let mut chunk = {
        let mut compiler = Compiler::new();
        compiler.compile_program(&ast);
        compiler.chunk
    };

    vm.run(&mut chunk)
}

fn main() {
    let mut report_channel = ReportChannel::new();
    let mut vm = VM::new();
    if let Some(filename) = ARGS.input() {
        match run_file(filename, &mut vm, &mut report_channel) {
            Err(err) => {
                ReportChannel::display(err.finish());
                if ARGS.repl() {
                    println!("Failed to run init file.")
                }
            }
            Ok(val) => dprintln!("{val}"),
        };
    }
    if ARGS.repl() || ARGS.input().is_none() {
        Repl::new(&mut vm, &report_channel).start_loop();
    }
}

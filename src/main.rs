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
    report_channel.check_reports_and_exit();
    dprintln!("{ast}");

    let mut chunk = {
        let mut compiler = Compiler::new(report_channel.get_sender());
        compiler.compile_program(&ast);
        compiler.chunk
    };
    report_channel.check_reports_and_exit();

    let v = vm.run(&mut chunk);
    vm.dump_stack();
    v
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
        Repl::new(&mut vm, &mut report_channel).start_loop();
    }
}

#[test]
fn run_tests() {
    let output = std::process::Command::new("python3")
        .args(&["./tester.py", "--compiler", "release", "./tests"])
        .output()
        .expect("Failed to run testing suite.");
    println!("{}", std::str::from_utf8(&output.stdout).unwrap());
    assert!(output.status.success());
}

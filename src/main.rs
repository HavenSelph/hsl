#![warn(clippy::complexity)]
#![allow(clippy::upper_case_acronyms)]

use crate::ast::parser::Parser;
use crate::compiler::Compiler;
use crate::repl::Repl;
use crate::report::{ReportChannel, UnwrapReport};
use crate::vm::VM;
use args::ARGS;

mod args;
mod ast;
mod compiler;
mod debug;
mod files;
mod repl;
mod report;
mod vm;

fn run_file(filename: &'static str, vm: &mut VM, report_channel: &mut ReportChannel) {
    let ast = Parser::new(filename, report_channel.get_sender())
        .unwrap_reported()
        .parse();
    report_channel.check_reports_and_exit();
    dprintln!("{ast}");

    let mut chunk = {
        let mut compiler = Compiler::new(report_channel.get_sender());
        compiler.compile_program(&ast);
        compiler.chunk
    };
    report_channel.check_reports_and_exit();
    vm.run(&mut chunk).unwrap_reported();
}

fn main() {
    let mut report_channel = ReportChannel::new();
    let mut vm = VM::new();
    if let Some(filename) = ARGS.input() {
        run_file(filename, &mut vm, &mut report_channel);
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

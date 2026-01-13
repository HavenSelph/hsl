#![warn(clippy::complexity)]
#![allow(clippy::upper_case_acronyms)]

use crate::ast::parser::Parser;
use crate::compiler::Compiler;
use crate::repl::Repl;
use crate::report::{ReportChannel, UnwrapReport};
use crate::vm::VM;
use args::{is_test_subcommand, ARGS};

mod args;
mod ast;
mod compiler;
mod debug;
mod files;
mod repl;
mod report;
mod tester;
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
    // Check if running test subcommand
    if let Some(test_args) = is_test_subcommand() {
        let config = tester::TesterConfig {
            compiler: test_args.compiler,
            paths: test_args.paths.into_iter().map(std::path::PathBuf::from).collect(),
            num_threads: test_args.threads,
        };
        std::process::exit(tester::run_tests(config));
    }

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
    let config = tester::TesterConfig {
        compiler: "release".to_string(),
        paths: vec![std::path::PathBuf::from("./tests")],
        num_threads: std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1),
    };
    let exit_code = tester::run_tests(config);
    assert_eq!(exit_code, 0, "Test suite failed");
}

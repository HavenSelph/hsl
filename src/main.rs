#![warn(clippy::complexity)]
#![allow(clippy::upper_case_acronyms)]

mod args;
mod ast;
mod compiler;
mod debug;
mod files;
mod report;
mod tester;
mod vm;

use crate::args::{ARGS, Commands};
use crate::ast::parser::Parser;
use crate::compiler::Compiler;
use crate::report::{ReportChannel, UnwrapReport};
use crate::vm::VM;

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
    let cli = &*ARGS;
    // Check if running test subcommand
    if let Some(Commands::Test(test_args)) = &cli.command {
        let mut paths = test_args.paths.clone();
        if paths.is_empty() {
            paths.push("tests".to_string());
        }
        let config = tester::TesterConfig {
            compiler: test_args.compiler.clone(),
            paths: paths.into_iter().map(std::path::PathBuf::from).collect(),
            num_threads: test_args.threads,
        };
        std::process::exit(tester::run_tests(config));
    }

    if ARGS.license {
        println!("{}", crate::args::LICENSE);
        std::process::exit(0);
    }

    let mut report_channel = ReportChannel::new();
    let mut vm = VM::new();
    if let Some(ref filename) = ARGS.input {
        run_file(
            Box::leak(filename.clone().into_boxed_str()),
            &mut vm,
            &mut report_channel,
        );
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

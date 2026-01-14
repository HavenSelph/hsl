use crate::report::ReportLevel;
use clap::{Parser, Subcommand};
use std::sync::LazyLock;

#[derive(Parser, Debug, Clone)]
#[command(name = "hsl")]
#[command(version, about = "HSL compiler and virtual machine.")]
pub struct ArgumentCLI {
    #[command(subcommand)]
    pub command: Option<Commands>,

    /// Filename to run
    #[arg(value_name = "FILE")]
    pub input: Option<String>,

    /// Set minimum level for a report to be shown
    #[arg(short = 'l', long, value_enum, default_value_t = ReportLevel::Warn)]
    pub report_level: ReportLevel,

    /// Show debug information (likely not useful for you)
    #[arg(short, long)]
    pub debug: bool,

    /// Display reports in one line
    #[arg(short, long)]
    pub compact: bool,

    /// Disable code context
    #[arg(long, default_value_t = true, action = clap::ArgAction::SetFalse)]
    pub context: bool,

    /// Print out the current op codes
    #[arg(long)]
    pub trace_execution: bool,

    /// Disable optimization passes
    #[arg(long)]
    pub no_optimize: bool,

    /// Set a maximum amount of reports to be printed
    #[arg(long, default_value_t = 25)]
    pub max_reports: usize,

    /// Show the license. (BSD 3-Clause)
    #[arg(short = 'L', long)]
    pub license: bool,
}

#[derive(Parser, Debug, Clone)]
pub struct TestArgs {
    /// Which compiler profile to use
    #[arg(short = 'i', long, default_value = "debug")]
    pub compiler: String,

    /// Number of parallel threads
    #[arg(short = 'c', long, default_value_t = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1))]
    pub threads: usize,

    /// Files or folders to test
    #[arg(value_name = "FILES/FOLDERS")]
    pub paths: Vec<String>,
}

#[derive(Subcommand, Debug, Clone)]
pub enum Commands {
    /// Run the test suite
    Test(TestArgs),
}

pub static ARGS: LazyLock<ArgumentCLI> = LazyLock::new(|| ArgumentCLI::parse());
pub const LICENSE: &str = include_str!("../LICENSE");

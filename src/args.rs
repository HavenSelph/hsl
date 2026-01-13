use crate::report::{ReportConfig, ReportKind, ReportLevel};
use std::process::exit;
use std::sync::LazyLock;

pub static ARGS: LazyLock<Args> = LazyLock::new(|| Args::parse(std::env::args().skip(1).collect()));

struct ArgParserReport(String);

impl ReportKind for ArgParserReport {
    fn title(&self) -> String {
        self.0.clone()
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }
}

macro_rules! error {
    ($($ident:tt)*) => {
        ArgParserReport(format!($($ident)*)).make().finish().write(std::io::stderr(), ReportConfig { compact: false, context: false });
        exit(1);
    };
}
#[derive(Copy, Clone)]
pub struct Arg<T: Copy + Clone> {
    pub value: T,
    set: bool,
}

impl<T: Copy + Clone> Arg<T> {
    fn new(default: T) -> Self {
        Self {
            value: default,
            set: false,
        }
    }

    fn try_mut<N: std::fmt::Display>(&mut self, name: N, value: T) {
        if self.set {
            error!("{} may only be used once", name);
        }
        self.set = true;
        self.value = value;
    }

    pub fn to_value(self) -> T {
        self.value
    }
}

macro_rules! args {
    ($name:ident; $($field:ident: $field_type:ty = ($field_default:expr)),+$(,)?) => {
        #[derive(Debug, Copy, Clone)]
        pub struct $name {
            $($field: Arg<$field_type>,)+
        }

        #[allow(dead_code)]
        impl $name {
            pub fn default() -> Self {
                Self {
                    $(
                    $field:Arg::new($field_default),
                    )+
                }
            }
            $(
            pub fn $field(&self) -> $field_type {
                self.$field.to_value()
            })+
        }
    };
}

impl<T: Copy + Clone> AsRef<T> for Arg<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T: std::fmt::Debug + Copy> std::fmt::Debug for Arg<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Arg({:?})", self.value)
    }
}

args! {
    Args;
    input: Option<&'static str>=(None),
    report_level: ReportLevel=(ReportLevel::Warn),
    repl: bool=(false),
    debug: bool=(false),
    compact: bool=(false),
    context: bool=(true),
    trace_execution: bool=(false),
    max_reports: usize=(25),
}

impl Args {
    fn handle_arg(
        &mut self,
        argument: &str,
        arguments: &mut std::iter::Peekable<std::vec::IntoIter<String>>,
    ) {
        let args: Vec<String> = if argument.starts_with("--") {
            vec![argument[2..].to_string()]
        } else {
            argument[1..].chars().map(|c| c.to_string()).collect()
        };
        let args_len = args.len();

        for (i, arg) in args.into_iter().enumerate() {
            let _is_end = i == args_len - 1;

            macro_rules! is_end {
                () => {
                    if !_is_end {
                        error!("{} may only be used at the end of a group", arg);
                    }
                };
            }

            match arg.as_str() {
                "h" => {
                    let usage = format!(
                        "{} {}",
                        std::env::current_exe()
                            .unwrap()
                            .file_name()
                            .unwrap()
                            .to_str()
                            .unwrap(),
                        USAGE
                    );
                    println!("Usage: {}", usage);
                    exit(0);
                }
                "help" => {
                    let usage = format!(
                        "{} {}",
                        std::env::current_exe()
                            .unwrap()
                            .file_name()
                            .unwrap()
                            .to_str()
                            .unwrap(),
                        USAGE
                    );
                    println!("\x1b[1mUSAGE\x1b[0m\n{}\n\n{}", usage, HELP_MESSAGE);
                    exit(0);
                }
                "V" | "version" => {
                    println!("hsl {}", env!("CARGO_PKG_VERSION"));
                    exit(0);
                }
                "L" | "license" => {
                    println!("{LICENSE}");
                    exit(0);
                }
                "r" | "repl" => self.repl.try_mut(arg, true),
                "d" | "debug" => self.debug.try_mut(arg, true),
                "c" | "compact" => self.compact.try_mut(arg, true),
                "disable-context" => self.context.try_mut(arg, false),
                "trace-execution" => self.trace_execution.try_mut(arg, true),
                "max-reports" => {
                    let Some(value) = arguments.next() else {
                        error!("{} expected NUMBER", arg);
                    };
                    let value = match value.parse::<usize>() {
                        Ok(value) => value,
                        Err(e) => {
                            error!("'{}' is not a valid NUMBER", e);
                        }
                    };
                    self.max_reports.try_mut(arg, value);
                }
                "l" | "report-level" => {
                    is_end!();
                    let Some(value) = arguments.next() else {
                        error!("{} expected LEVEL", arg);
                    };
                    let level = match value.as_str() {
                        "advice" => ReportLevel::Advice,
                        "warn" => ReportLevel::Warn,
                        "error" => ReportLevel::Error,
                        "silent" => ReportLevel::Silent,
                        _ => {
                            error!("'{}' is not a valid LEVEL", value);
                        }
                    };
                    self.report_level.try_mut(arg, level);
                }
                _ => {
                    error!("unrecognized argument {}, -h for usage", arg);
                }
            }
        }
    }

    pub fn parse(args: Vec<String>) -> Self {
        let mut out = Self::default();
        let mut args = args.into_iter().peekable();
        while let Some(arg) = args.next() {
            if arg.starts_with("-") {
                out.handle_arg(&arg, &mut args)
            } else {
                out.input.try_mut("Filename", Some(arg.leak()));
                break;
            }
        }
        if let Some(arg) = args.next() {
            error!("unexpected argument '{}', -h for usage", arg);
        }

        out
    }
}

/// Arguments for the test subcommand
#[derive(Debug, Clone)]
pub struct TestArgs {
    pub compiler: String,
    pub paths: Vec<String>,
    pub threads: usize,
}

impl TestArgs {
    pub fn parse(args: Vec<String>) -> Self {
        let mut compiler = "debug".to_string();
        let mut paths: Vec<String> = Vec::new();
        let mut threads = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);

        let mut args = args.into_iter().peekable();
        while let Some(arg) = args.next() {
            if arg.starts_with("-") {
                match arg.as_str() {
                    "-h" | "--help" => {
                        println!("{}", TEST_HELP_MESSAGE);
                        exit(0);
                    }
                    "-i" | "--compiler" => {
                        if let Some(value) = args.next() {
                            compiler = value;
                        } else {
                            error!("{} expected COMPILER", arg);
                        }
                    }
                    "-c" | "--cpus" => {
                        if let Some(value) = args.next() {
                            match value.parse::<usize>() {
                                Ok(n) => threads = n,
                                Err(_) => {
                                    error!("'{}' is not a valid number", value);
                                }
                            }
                        } else {
                            error!("{} expected NUMBER", arg);
                        }
                    }
                    _ => {
                        error!("unrecognized argument {}, --help for usage", arg);
                    }
                }
            } else {
                paths.push(arg);
            }
        }

        if paths.is_empty() {
            paths.push("tests".to_string());
        }

        Self {
            compiler,
            paths,
            threads,
        }
    }
}

/// Check if the first argument is "test" subcommand
pub fn is_test_subcommand() -> Option<TestArgs> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.first().map(|s| s.as_str()) == Some("test") {
        Some(TestArgs::parse(args.into_iter().skip(1).collect()))
    } else {
        None
    }
}

const LICENSE: &str = include_str!("../LICENSE");
const USAGE: &str = "[test] <INPUT FILE>";
const HELP_MESSAGE: &str = "\x1b[1mOPTIONS\x1b[0m
    -h, --help                        Show this message (or only usage with -h)
    -V, --version
    -L, --license                     Show the license. (BSD 3-Clause)

    -r, --repl                        Run the repl, you may also pass a file
    -l, --report-level LEVEL          Set minimum level for a report to be shown
       (default: error)               [advice|warn|error|silent]
    -d, --debug                       Show debug information (likely not useful for you)
    -c, --compact                     Display reports in one line

        --disable-context             Disable code context
        --trace-execution             Print out the current op codes
        --max-reports                 Set a maximum amount of reports to be printed
        (default: 25)

\x1b[1mSUBCOMMANDS\x1b[0m
    test                              Run the test suite (use 'hsl test --help' for more)
";

const TEST_HELP_MESSAGE: &str = "\x1b[1mUSAGE\x1b[0m
hsl test [OPTIONS] [FILES/FOLDERS...]

\x1b[1mOPTIONS\x1b[0m
    -h, --help                        Show this message
    -i, --compiler COMPILER           Which compiler profile to use (default: debug)
    -c, --cpus NUMBER                 Number of parallel threads (default: CPU count)

\x1b[1mARGUMENTS\x1b[0m
    [FILES/FOLDERS...]                Files or folders to test (default: tests)
";

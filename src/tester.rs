// NOT UNDER MY LICENSE
// Original Author:
// https://github.com/mustafaquraish/aecor/blob/master/meta/test.py
// Translated to Rust from tester.py

use owo_colors::OwoColorize;
use std::io::{BufRead, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedResult {
    ExitWithCode(i32),
    ExitWithOutput(String),
    ExitWithOutputAndBytecode { output: String, bytecode: Vec<String> },
    Fail(String),
    SkipSilently,
    SkipReport,
}

#[derive(Debug, Clone)]
pub struct Expected {
    pub result_type: ExpectedResult,
}

impl Expected {
    fn new(result_type: ExpectedResult) -> Self {
        Self { result_type }
    }
}

#[derive(Debug)]
pub struct TestResult {
    pub passed: bool,
    pub message: String,
    pub path: PathBuf,
}

/// Parse the expected result from a test file.
/// Test files use `///` comments at the start to declare expectations:
/// - `/// skip` - Skip the test silently
/// - `/// exit: <code>` - Expect exit with specific code
/// - `/// out: <output>` - Expect specific output (can be multiline)
/// - `/// fail: <error>` - Expect failure with specific error message
pub fn get_expected(filename: &Path) -> Option<Expected> {
    let file = match std::fs::File::open(filename) {
        Ok(f) => f,
        Err(_) => return None,
    };

    let reader = std::io::BufReader::new(file);
    let mut lines = reader.lines();

    while let Some(Ok(line)) = lines.next() {
        if !line.starts_with("///") {
            break;
        }

        let line = line[3..].trim().to_string();

        // Commands with no arguments
        if line == "skip" {
            return Some(Expected::new(ExpectedResult::SkipSilently));
        }
        if line.is_empty() {
            continue;
        }

        if !line.contains(':') {
            eprintln!(
                "[-] Invalid parameters in {}: \"{}\"",
                filename.display(),
                line
            );
            break;
        }

        // Commands with arguments
        let parts: Vec<&str> = line.splitn(2, ':').collect();
        let name = parts[0].trim();
        let value = parts[1].trim();

        match name {
            "exit" => {
                if let Ok(code) = value.parse::<i32>() {
                    return Some(Expected::new(ExpectedResult::ExitWithCode(code)));
                } else {
                    eprintln!("[-] Invalid exit code in {}: {}", filename.display(), value);
                    break;
                }
            }
            "out" => {
                let mut output_value = value.to_string();
                if output_value.is_empty() {
                    // Multi-line output - read subsequent lines starting with ///
                    let mut multiline = String::new();
                    for next_line in lines.by_ref() {
                        match next_line {
                            Ok(ln) if ln.starts_with("///") => {
                                if !multiline.is_empty() {
                                    multiline.push('\n');
                                }
                                multiline.push_str(ln[3..].trim());
                            }
                            _ => break,
                        }
                    }
                    output_value = multiline;
                }
                // Parse the string value (handle escape sequences and quotes)
                let parsed_value = parse_string_literal(&output_value);
                return Some(Expected::new(ExpectedResult::ExitWithOutput(parsed_value)));
            }
            "fail" => {
                return Some(Expected::new(ExpectedResult::Fail(value.to_string())));
            }
            "bc" => {
                // Bytecode verification - collect expected opcodes
                let mut bytecode_patterns = vec![value.to_string()];
                for next_line in lines.by_ref() {
                    match next_line {
                        Ok(ln) if ln.starts_with("///") => {
                            let content = ln[3..].trim();
                            if content.starts_with("bc:") {
                                bytecode_patterns.push(content[3..].trim().to_string());
                            } else if content.starts_with("out:") {
                                // Found output section, parse it
                                let mut output_value = content[4..].trim().to_string();
                                if output_value.is_empty() {
                                    let mut multiline = String::new();
                                    for out_line in lines.by_ref() {
                                        match out_line {
                                            Ok(ol) if ol.starts_with("///") => {
                                                if !multiline.is_empty() {
                                                    multiline.push('\n');
                                                }
                                                multiline.push_str(ol[3..].trim());
                                            }
                                            _ => break,
                                        }
                                    }
                                    output_value = multiline;
                                }
                                let parsed_output = parse_string_literal(&output_value);
                                return Some(Expected::new(ExpectedResult::ExitWithOutputAndBytecode {
                                    output: parsed_output,
                                    bytecode: bytecode_patterns,
                                }));
                            } else {
                                break;
                            }
                        }
                        _ => break,
                    }
                }
                // No output section found, just check bytecode with exit 0
                return Some(Expected::new(ExpectedResult::ExitWithOutputAndBytecode {
                    output: String::new(),
                    bytecode: bytecode_patterns,
                }));
            }
            _ => {
                eprintln!("[-] Invalid parameter in {}: {}", filename.display(), line);
                break;
            }
        }
    }

    Some(Expected::new(ExpectedResult::SkipReport))
}

/// Parse a Python-style string literal (handles quotes and escape sequences)
fn parse_string_literal(s: &str) -> String {
    let s = s.trim();

    // Handle triple-quoted strings
    if s.starts_with("\"\"\"") && s.ends_with("\"\"\"") && s.len() >= 6 {
        return s[3..s.len() - 3].to_string();
    }

    // Handle single-quoted or double-quoted strings
    if (s.starts_with('"') && s.ends_with('"')) || (s.starts_with('\'') && s.ends_with('\'')) {
        if s.len() >= 2 {
            let inner = &s[1..s.len() - 1];
            return parse_escape_sequences(inner);
        }
    }

    // Return as-is if no quotes
    s.to_string()
}

/// Parse escape sequences in a string
fn parse_escape_sequences(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('\'') => result.push('\''),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Run a single test and return the result
pub fn handle_test(compiler: &Path, path: &Path, expected: &Expected) -> TestResult {
    let needs_debug = matches!(
        expected.result_type,
        ExpectedResult::ExitWithOutputAndBytecode { .. }
    );
    
    let mut args = vec!["--compact".to_string()];
    if needs_debug {
        args.push("--debug".to_string());
    }
    args.push(path.to_string_lossy().to_string());
    
    let output = Command::new(compiler)
        .args(&args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output();

    let output = match output {
        Ok(o) => o,
        Err(e) => {
            return TestResult {
                passed: false,
                message: format!("Failed to run compiler: {}", e),
                path: path.to_path_buf(),
            };
        }
    };

    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
    let exit_code = output.status.code().unwrap_or(-1);

    match &expected.result_type {
        ExpectedResult::Fail(expected_error) => {
            if exit_code == 0 {
                return TestResult {
                    passed: false,
                    message: "Expected failure, but succeeded".to_string(),
                    path: path.to_path_buf(),
                };
            }

            if &stderr == expected_error || stderr.contains(expected_error) {
                return TestResult {
                    passed: true,
                    message: "(Success)".to_string(),
                    path: path.to_path_buf(),
                };
            }

            // Try to extract just the error message
            let remaining = stderr
                .lines()
                .next()
                .and_then(|line| line.split("Error: ").nth(1))
                .unwrap_or(&stderr);

            TestResult {
                passed: false,
                message: format!(
                    "Did not find expected error message\nexpected: {:?}\ngot: {:?}",
                    expected_error, remaining
                ),
                path: path.to_path_buf(),
            }
        }
        ExpectedResult::ExitWithCode(expected_code) => {
            if exit_code != *expected_code {
                TestResult {
                    passed: false,
                    message: format!(
                        "Expected exit code {}, but got {}\n{}",
                        expected_code, exit_code, stderr
                    ),
                    path: path.to_path_buf(),
                }
            } else {
                TestResult {
                    passed: true,
                    message: "(Success)".to_string(),
                    path: path.to_path_buf(),
                }
            }
        }
        ExpectedResult::ExitWithOutput(expected_output) => {
            if exit_code != 0 {
                return TestResult {
                    passed: false,
                    message: format!("Expected exit code 0, but got {}\n{}", exit_code, stderr),
                    path: path.to_path_buf(),
                };
            }

            let expected_out = expected_output.trim();
            if stdout != expected_out {
                TestResult {
                    passed: false,
                    message: format!(
                        "Incorrect output produced\nexpected: {:?}\ngot: {:?}",
                        expected_out, stdout
                    ),
                    path: path.to_path_buf(),
                }
            } else {
                TestResult {
                    passed: true,
                    message: "(Success)".to_string(),
                    path: path.to_path_buf(),
                }
            }
        }
        ExpectedResult::ExitWithOutputAndBytecode { output: expected_output, bytecode } => {
            if exit_code != 0 {
                return TestResult {
                    passed: false,
                    message: format!("Expected exit code 0, but got {}\n{}", exit_code, stderr),
                    path: path.to_path_buf(),
                };
            }

            // Check bytecode patterns in stdout (debug output goes to stdout)
            let debug_output = &stdout;
            let mut missing_patterns = Vec::new();
            for pattern in bytecode {
                if !debug_output.contains(pattern) {
                    missing_patterns.push(pattern.clone());
                }
            }

            if !missing_patterns.is_empty() {
                return TestResult {
                    passed: false,
                    message: format!(
                        "Missing expected bytecode patterns:\n  {}\nActual debug output:\n{}",
                        missing_patterns.join("\n  "),
                        debug_output
                    ),
                    path: path.to_path_buf(),
                };
            }

            // Check output - extract the last line(s) after the bytecode dump
            // The actual program output comes after the "X instructions and Y bytes" line
            let expected_out = expected_output.trim();
            if !expected_out.is_empty() {
                // Find where the actual output starts (after bytecode dump)
                let output_lines: Vec<&str> = stdout.lines().collect();
                let mut actual_output = String::new();
                let mut found_bytecode_end = false;
                for line in &output_lines {
                    if found_bytecode_end {
                        if !actual_output.is_empty() {
                            actual_output.push('\n');
                        }
                        actual_output.push_str(line);
                    } else if line.contains("instructions and") && line.contains("bytes") {
                        found_bytecode_end = true;
                    }
                }
                
                if actual_output.trim() != expected_out {
                    return TestResult {
                        passed: false,
                        message: format!(
                            "Incorrect output produced\nexpected: {:?}\ngot: {:?}",
                            expected_out, actual_output.trim()
                        ),
                        path: path.to_path_buf(),
                    };
                }
            }

            TestResult {
                passed: true,
                message: "(Success)".to_string(),
                path: path.to_path_buf(),
            }
        }
        ExpectedResult::SkipSilently | ExpectedResult::SkipReport => TestResult {
            passed: true,
            message: "(Skipped)".to_string(),
            path: path.to_path_buf(),
        },
    }
}

/// Collect all test files from a path (file or directory)
fn collect_test_files(path: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();

    if path.is_dir() {
        if let Ok(entries) = walkdir(path) {
            for entry in entries {
                if entry.extension().map_or(false, |ext| ext == "hsl") {
                    files.push(entry);
                }
            }
        }
    } else if path.is_file() {
        files.push(path.to_path_buf());
    }

    files
}

/// Walk directory recursively
fn walkdir(dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            files.extend(walkdir(&path)?);
        } else if path.is_file() {
            files.push(path);
        }
    }

    Ok(files)
}

#[derive(Debug, Clone)]
pub struct TesterConfig {
    pub compiler: String,
    pub paths: Vec<PathBuf>,
    pub num_threads: usize,
}

impl Default for TesterConfig {
    fn default() -> Self {
        Self {
            compiler: "debug".to_string(),
            paths: vec![PathBuf::from("tests")],
            num_threads: thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(1),
        }
    }
}

/// Main entry point for running tests
pub fn run_tests(config: TesterConfig) -> i32 {
    // Build the compiler
    let compiler_flag = format!("--{}", config.compiler);
    let build_args: Vec<&str> = if config.compiler != "debug" {
        vec!["build", "--quiet", &compiler_flag]
    } else {
        vec!["build", "--quiet"]
    };

    let build_status = Command::new("cargo")
        .args(&build_args)
        .status()
        .expect("Failed to run cargo build");

    if !build_status.success() {
        eprintln!("[-] Cargo build failed");
        return 1;
    }

    // Determine compiler path
    let cwd = std::env::current_dir().expect("Failed to get current directory");
    let mut compiler_path = cwd.join("target").join(&config.compiler).join("hsl");

    // Check for Windows .exe extension
    if compiler_path.with_extension("exe").exists() {
        println!("[+] Assuming on Windows, .exe found");
        compiler_path = compiler_path.with_extension("exe");
    } else {
        println!("[+] Assuming on Linux/macOS, no .exe found");
    }

    if !compiler_path.exists() {
        eprintln!("[-] Interpreter {} not found", compiler_path.display());
        return 1;
    }

    // Collect all test files
    let mut tests_to_run: Vec<(PathBuf, Expected)> = Vec::new();

    for path in &config.paths {
        let files = collect_test_files(path);

        for file in files {
            if let Some(expected) = get_expected(&file) {
                match expected.result_type {
                    ExpectedResult::SkipSilently => continue,
                    ExpectedResult::SkipReport => {
                        println!("[-] Skipping {}", file.display());
                        continue;
                    }
                    _ => tests_to_run.push((file, expected)),
                }
            }
        }
    }

    let num_total = tests_to_run.len();
    let num_passed = Arc::new(AtomicUsize::new(0));
    let num_failed = Arc::new(AtomicUsize::new(0));
    let failed_tests: Arc<Mutex<Vec<(PathBuf, String)>>> = Arc::new(Mutex::new(Vec::new()));

    // Create a thread pool
    let tests_to_run = Arc::new(Mutex::new(tests_to_run));
    let compiler_path = Arc::new(compiler_path);

    let mut handles = Vec::new();

    for _ in 0..config.num_threads {
        let tests = Arc::clone(&tests_to_run);
        let compiler = Arc::clone(&compiler_path);
        let passed = Arc::clone(&num_passed);
        let failed = Arc::clone(&num_failed);
        let failures = Arc::clone(&failed_tests);

        let handle = thread::spawn(move || {
            loop {
                let test = {
                    let mut tests_guard = tests.lock().unwrap();
                    tests_guard.pop()
                };

                match test {
                    Some((path, expected)) => {
                        let result = handle_test(&compiler, &path, &expected);

                        if result.passed {
                            passed.fetch_add(1, Ordering::SeqCst);
                        } else {
                            failed.fetch_add(1, Ordering::SeqCst);
                            let mut failures_guard = failures.lock().unwrap();
                            failures_guard.push((result.path, result.message));
                        }
                    }
                    None => break,
                }
            }
        });

        handles.push(handle);
    }

    // Progress reporting in main thread
    let passed_ref = Arc::clone(&num_passed);
    let failed_ref = Arc::clone(&num_failed);

    loop {
        let current_passed = passed_ref.load(Ordering::SeqCst);
        let current_failed = failed_ref.load(Ordering::SeqCst);
        let finished = current_passed + current_failed;

        print!(
            " \x1b[2K[{}/{}] Running tests, finished {} / {}\r",
            current_passed.green(),
            current_failed.red(),
            finished,
            num_total
        );
        std::io::stdout().flush().unwrap();

        if finished >= num_total {
            break;
        }

        thread::sleep(std::time::Duration::from_millis(50));
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().unwrap();
    }

    println!("\x1b[2K");

    // Print failed tests
    let failures = failed_tests.lock().unwrap();
    for (path, message) in failures.iter() {
        println!(
            "{} {}",
            "[-] Failed".red(),
            path.canonicalize()
                .unwrap_or_else(|_| path.clone())
                .display()
        );
        let indented_message = message.replace('\n', "\n      ");
        println!("    {}", indented_message);
    }

    let final_passed = num_passed.load(Ordering::SeqCst);
    let final_failed = num_failed.load(Ordering::SeqCst);

    println!("Tests passed: {}", final_passed.green());
    println!("Tests failed: {}", final_failed.red());

    if final_failed > 0 { 1 } else { 0 }
}

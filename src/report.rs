#![allow(unused)]
use crate::args::ARGS;
use crate::ast::span::Span;
use crate::files::get_source;
use name_variant::NamedVariant;
use owo_colors::colors::CustomColor;
use owo_colors::{AnsiColors, DynColors, OwoColorize};
use std::fmt::Display;
use std::io;
use std::io::Write;
use std::process::exit;
use std::sync::mpsc::{Receiver, Sender};

pub type Maybe<T> = Result<T, Box<ReportBuilder>>;
pub type MaybeFinal<T> = Result<T, Box<Report>>;
pub type MaybeErrorless<T> = Result<T, ()>;

#[derive(Clone)]
pub struct Label {
    span: Span,
    color: Option<AnsiColors>,
    front_message: Option<String>,
    back_message: Option<String>,
}

impl Label {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            color: None,
            front_message: None,
            back_message: None,
        }
    }

    pub fn set_color(&mut self, color: AnsiColors) -> &mut Self {
        self.color = Some(color);
        self
    }

    pub fn with_color(mut self, color: AnsiColors) -> Self {
        self.set_color(color);
        self
    }

    pub fn set_front_message<T: Display>(&mut self, message: T) -> &mut Self {
        self.front_message = Some(message.to_string());
        self
    }

    pub fn with_front_message<T: Display>(mut self, message: T) -> Self {
        self.set_front_message(message);
        self
    }

    pub fn set_back_message<T: Display>(&mut self, message: T) -> &mut Self {
        self.back_message = Some(message.to_string());
        self
    }

    pub fn with_back_message<T: Display>(mut self, message: T) -> Self {
        self.set_back_message(message);
        self
    }

    /// Convenience method to set the back message (most common use case)
    pub fn set_message<T: Display>(&mut self, message: T) -> &mut Self {
        self.set_back_message(message)
    }

    pub fn with_message<T: Display>(mut self, message: T) -> Self {
        self.set_back_message(message);
        self
    }
}

pub trait SpanToLabel {
    fn label(&self) -> Label;

    fn labeled<M: Display>(&self, message: M) -> Label {
        self.label().with_message(message)
    }
}

impl SpanToLabel for Span {
    fn label(&self) -> Label {
        Label::new(*self)
    }
}

pub trait UnwrapReport<T>
where
    Self: Sized,
{
    fn unwrap_reported(self) -> T;
}

impl<T> UnwrapReport<T> for Maybe<T> {
    fn unwrap_reported(self) -> T {
        match self {
            Ok(val) => val,
            Err(err) => {
                let err = err.finish();
                ReportChannel::should_display(&err).then(|| err.eprint(ReportConfig::default()));
                exit(1);
            }
        }
    }
}

impl<T> UnwrapReport<T> for MaybeFinal<T> {
    fn unwrap_reported(self) -> T {
        match self {
            Ok(val) => val,
            Err(err) => {
                ReportChannel::should_display(&*err).then(|| err.eprint(ReportConfig::default()));
                exit(1);
            }
        }
    }
}

pub trait ReportKind
where
    Self: Sized,
{
    fn title(&self) -> String;
    fn level(&self) -> ReportLevel;

    fn make(self) -> ReportBuilder {
        ReportBuilder {
            title: self.title(),
            level: self.level(),
            help: None,
            note: None,
            label: None,
            incomplete: self.incomplete(),
        }
    }

    fn make_labeled(self, label: Label) -> ReportBuilder {
        self.make().with_label(label)
    }

    fn incomplete(self) -> bool {
        false
    }
}

use clap::ValueEnum;

#[derive(ValueEnum, NamedVariant, Debug, Copy, Clone, PartialOrd, PartialEq)]
pub enum ReportLevel {
    Silent,
    Error,
    Warning,
    Advice,
}

impl ReportLevel {
    fn color(&self) -> AnsiColors {
        match self {
            ReportLevel::Advice => AnsiColors::Blue,
            ReportLevel::Warning => AnsiColors::Yellow,
            ReportLevel::Error => AnsiColors::Red,
            ReportLevel::Silent => unreachable!(),
        }
    }
}

#[must_use]
pub struct ReportBuilder {
    pub level: ReportLevel,
    pub title: String,
    pub help: Option<String>,
    pub note: Option<String>,
    pub label: Option<Label>,
    pub incomplete: bool,
}

impl ReportBuilder {
    pub fn set_help<T: Display>(&mut self, help: T) -> &mut Self {
        self.help = Some(help.to_string());
        self
    }

    pub fn with_help<T: Display>(mut self, help: T) -> Self {
        self.set_help(help);
        self
    }

    pub fn set_note<T: Display>(&mut self, note: T) -> &mut Self {
        self.note = Some(note.to_string());
        self
    }

    pub fn with_note<T: Display>(mut self, note: T) -> Self {
        self.set_note(note);
        self
    }

    pub fn set_label(&mut self, label: Label) -> &mut Self {
        self.label = Some(label);
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.set_label(label);
        self
    }

    pub fn set_incomplete(&mut self) -> &mut Self {
        self.incomplete = true;
        self
    }

    pub fn incomplete(mut self) -> Self {
        self.set_incomplete();
        self
    }

    pub fn finish(self) -> Report {
        Report {
            level: self.level,
            title: self.title,
            help: self.help,
            note: self.note,
            label: self.label,
            incomplete: self.incomplete,
        }
    }
}

#[derive(Copy, Clone)]
pub struct ReportConfig {
    pub compact: bool,
    pub context: bool,
}

impl Default for ReportConfig {
    fn default() -> Self {
        Self {
            compact: ARGS.compact,
            context: ARGS.context,
        }
    }
}

#[derive(Clone)]
pub struct Report {
    pub level: ReportLevel,
    title: String,
    help: Option<String>,
    note: Option<String>,
    label: Option<Label>,
    pub incomplete: bool,
}

impl Report {
    /// Writes only the source context lines for a label's span.
    /// Handles multi-line spans with truncation (shows first 2 and last 2 lines if > 4 lines).
    fn write_source_context<W: Write>(&self, dst: &mut W, label: &Label, line_num_width: usize) {
        let span = label.span;
        let source = match get_source(span.filename) {
            Ok(s) => s,
            Err(_) => return,
        };

        let start_loc = span.start_location();
        let end_loc = span.end_location();
        let start_line = start_loc.line;
        let end_line = end_loc.line;
        let color = label.color.unwrap_or_else(|| self.level.color());
        let pipe = "│".bright_black();

        // Determine which lines to show
        let total_lines = end_line - start_line + 1;
        let lines_to_show: Vec<usize> = if total_lines <= 4 {
            (start_line..=end_line).collect()
        } else {
            // Show first 2 and last 2 lines
            vec![start_line, start_line + 1, end_line - 1, end_line]
        };

        let mut prev_line: Option<usize> = None;
        let mut is_first_line = true;
        let mut is_last_line;

        for (idx, line_num) in lines_to_show.iter().enumerate() {
            is_last_line = idx == lines_to_show.len() - 1;

            // Check if we need to show "N lines hidden..." message
            if let Some(prev) = prev_line {
                if *line_num > prev + 1 {
                    let hidden = line_num - prev - 1;
                    let _ = writeln!(
                        dst,
                        "{}",
                        format!(
                            " {:>width$} │ {} lines hidden...",
                            "",
                            hidden,
                            width = line_num_width
                        )
                        .bright_black()
                    );
                }
            }

            let Some(line_content) = source.get_line(*line_num) else {
                prev_line = Some(*line_num);
                continue;
            };

            // Write front message before first line if present
            if is_first_line {
                if let Some(ref msg) = label.front_message {
                    let _ = writeln!(
                        dst,
                        " {:>width$} {} {}",
                        "",
                        "╭".color(color),
                        msg.color(color),
                        width = line_num_width
                    );
                }
            }

            // Write the source line
            let _ = write!(
                dst,
                " {:>width$} {} ",
                line_num.bright_black(),
                pipe,
                width = line_num_width
            );

            // Determine highlighting range for this line
            let (highlight_start, highlight_end) =
                if *line_num == start_line && *line_num == end_line {
                    // Single line span
                    (start_loc.column, end_loc.column)
                } else if *line_num == start_line {
                    // First line of multi-line span
                    (start_loc.column, line_content.len() + 1)
                } else if *line_num == end_line {
                    // Last line of multi-line span
                    (1, end_loc.column)
                } else {
                    // Middle line - highlight entire line
                    (1, line_content.len() + 1)
                };

            // Print the line with highlighting
            for (i, ch) in line_content.chars().enumerate() {
                let col = i + 1;
                if col >= highlight_start && col < highlight_end {
                    let _ = write!(dst, "{}", ch.to_string().color(color));
                } else {
                    let _ = write!(dst, "{}", ch);
                }
            }
            let _ = writeln!(dst);

            // Write back message after last line if present
            if is_last_line {
                if let Some(ref msg) = label.back_message {
                    let _ = writeln!(
                        dst,
                        " {:>width$} {} {}",
                        "",
                        "╰".color(color),
                        msg.color(color),
                        width = line_num_width
                    );
                }
            }

            prev_line = Some(*line_num);
            is_first_line = false;
        }
    }

    pub fn write<W: Write>(self, mut dst: W, config: ReportConfig) {
        // Write the header: "Error: message" or "Warning: message"
        let header = format!("{}:", self.level.variant_name());
        let _ = write!(dst, "{} ", header.color(self.level.color()));

        // For compact mode, include span in header
        if config.compact {
            if let Some(ref label) = self.label {
                let _ = write!(dst, "[ {} ] ", label.span);
            }
        }

        let _ = writeln!(dst, "{}", self.title);

        if config.compact {
            return;
        }

        // Write source context if enabled and we have a label
        if let Some(ref label) = self.label {
            // Calculate line number width based on the end line
            let end_line = label.span.end_location().line;
            let line_num_width = end_line.to_string().len().max(3);

            // Header with location
            let _ = writeln!(
                dst,
                "  {:>width$}{}{}{}",
                "",
                "╭─[".bright_black(),
                format!(" {} ", label.span),
                "]".bright_black(),
                width = line_num_width
            );

            // Empty line with pipe
            let _ = writeln!(
                dst,
                " {:>width$} {}",
                "",
                "│".bright_black(),
                width = line_num_width
            );

            if config.context {
                self.write_source_context(&mut dst, label, line_num_width);
            }

            // Footer
            let _ = writeln!(
                dst,
                "{}",
                format!("{}╯", "─".repeat(line_num_width + 2)).bright_black()
            );
        }

        // Write help if present
        if let Some(help) = self.help {
            let _ = writeln!(
                dst,
                "  {} {}: {}",
                "│".bright_black(),
                "Help".fg::<CustomColor<132, 209, 172>>(),
                help
            );
        }

        // Write note if present
        if let Some(note) = self.note {
            let _ = writeln!(
                dst,
                "  {} {}: {}",
                "│".bright_black(),
                "Note".fg::<CustomColor<132, 209, 172>>(),
                note
            );
        }
    }

    pub fn print(self, config: ReportConfig) {
        self.write(io::stdout(), config);
    }

    pub fn eprint(self, config: ReportConfig) {
        self.write(io::stderr(), config);
    }
}

pub enum ExitStatus {
    No,
    Yes,
}

pub struct ReportChannel {
    reported: usize,
    pub sender: Sender<Box<Report>>,
    pub receiver: Receiver<Box<Report>>,
}

#[derive(Clone)]
pub struct ReportSender {
    sender: Sender<Box<Report>>,
}

impl ReportSender {
    pub fn report(&self, report: Box<Report>) {
        self.sender.send(report).expect("Failed to send report");
    }
}

impl ReportChannel {
    pub fn new() -> ReportChannel {
        let (sender, receiver) = std::sync::mpsc::channel();
        ReportChannel {
            reported: 0,
            sender,
            receiver,
        }
    }

    pub fn get_sender(&self) -> ReportSender {
        ReportSender {
            sender: self.sender.clone(),
        }
    }

    pub fn should_display(report: &Report) -> bool {
        ARGS.report_level >= report.level
    }

    pub fn display(report: Report) {
        if Self::should_display(&report) {
            report.eprint(ReportConfig::default());
        }
    }

    pub fn check_reports(&mut self) -> ExitStatus {
        let mut errors = 0usize;
        let config = ReportConfig::default();
        for report in self.receiver.try_iter() {
            if report.level == ReportLevel::Error {
                errors += 1;
            }
            if !Self::should_display(&*report) || self.reported == ARGS.max_reports {
                continue;
            }
            report.eprint(config);
            self.reported += 1;
        }
        if errors > 0 {
            if ARGS.report_level != ReportLevel::Silent {
                eprintln!(
                    "{}",
                    format_args!("Failed with {errors} errors emitted.").red()
                );
            }
            ExitStatus::Yes
        } else {
            ExitStatus::No
        }
    }

    pub fn check_reports_and_exit(&mut self) {
        match self.check_reports() {
            ExitStatus::Yes => exit(1),
            ExitStatus::No => (),
        }
    }
}

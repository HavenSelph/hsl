#![allow(unused)]
use crate::args::ARGS;
use crate::ast::span::Span;
use crate::files::ScannerCache;
use ariadne::{Color, Config};
use name_variant::NamedVariant;
use owo_colors::colors::CustomColor;
use owo_colors::{AnsiColors, OwoColorize};
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
    message: Option<String>,
    color: Option<Color>,
    priority: Option<i32>,
}

impl Label {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            message: None,
            color: None,
            priority: None,
        }
    }
    pub fn set_message<T: Display>(&mut self, message: T) -> &mut Self {
        self.message = Some(message.to_string());
        self
    }

    pub fn with_message<T: Display>(mut self, message: T) -> Self {
        self.set_message(message);
        self
    }
    pub fn set_color(&mut self, color: Color) -> &mut Self {
        self.color = Some(color);
        self
    }

    pub fn with_color(mut self, color: Color) -> Self {
        self.set_color(color);
        self
    }

    pub fn set_priority(&mut self, priority: i32) -> &mut Self {
        self.priority = Some(priority);
        self
    }

    pub fn with_priority(mut self, priority: i32) -> Self {
        self.set_priority(priority);
        self
    }

    fn as_ariadne_label(&self, level: ReportLevel) -> ariadne::Label<Span> {
        let mut label =
            ariadne::Label::new(self.span).with_color(if let Some(color) = self.color {
                color
            } else {
                level.into()
            });
        if let Some(text) = self.message.clone() {
            label = label.with_message(text);
        }
        if let Some(priority) = self.priority {
            label = label.with_priority(priority)
        }
        label
    }
}
pub trait SpanToLabel<T: ariadne::Span>: ariadne::Span {
    fn label(&self) -> Label;

    fn labeled<M: Display>(&self, message: M) -> Label {
        self.label().with_message(message)
    }
}

impl SpanToLabel<Span> for Span {
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
            labels: Vec::new(),
            incomplete: self.incomplete(),
        }
    }

    fn make_labeled(self, label: Label) -> ReportBuilder {
        self.make().with_label(label.with_priority(1))
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
    Warn,
    Advice,
}

impl From<ReportLevel> for ariadne::ReportKind<'_> {
    fn from(value: ReportLevel) -> Self {
        match value {
            ReportLevel::Error => Self::Error,
            ReportLevel::Warn => Self::Warning,
            ReportLevel::Advice => Self::Advice,
            ReportLevel::Silent => panic!("Turned SILENT into report kind"),
        }
    }
}

impl From<ReportLevel> for Color {
    fn from(value: ReportLevel) -> Self {
        match value {
            ReportLevel::Advice => Color::BrightBlue,
            ReportLevel::Warn => Color::Yellow,
            ReportLevel::Error => Color::Red,
            ReportLevel::Silent => panic!("Turned SILENT into color"),
        }
    }
}

#[must_use]
pub struct ReportBuilder {
    pub level: ReportLevel,
    pub title: String,
    pub help: Option<String>,
    pub note: Option<String>,
    pub labels: Vec<Label>,
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

    pub fn push_label(&mut self, label: Label) -> &mut Self {
        self.labels.push(label);
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.push_label(label);
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
            labels: self.labels,
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
    labels: Vec<Label>,
    pub incomplete: bool,
}

impl Report {
    fn into_ariadne_report(self) -> ariadne::Report<'static, Span> {
        let mut builder = ariadne::Report::build(
            self.level.into(),
            self.labels
                .first()
                .expect("Context report invoked on non-spanned error")
                .span,
        )
        .with_message(self.title)
        .with_config(Config::default().with_compact(true))
        .with_labels(
            self.labels
                .iter()
                .map(|label| label.as_ariadne_label(self.level)),
        );
        if let Some(help) = self.help {
            builder.set_help(help);
        }
        if let Some(note) = self.note {
            builder.set_note(note);
        }
        builder.finish()
    }

    pub fn write<W: Write>(self, mut dst: W, config: ReportConfig) {
        if !config.compact && (config.context && !self.labels.is_empty()) {
            self.into_ariadne_report()
                .write(ScannerCache {}, dst)
                .expect("Failed to write error via ariadne.");
            return;
        }

        let compact_span = config.compact || (self.note.is_none() && self.help.is_none());
        writeln!(
            dst,
            "{} {}",
            format!(
                "{}{}:",
                if compact_span && !self.labels.is_empty() {
                    format!("[{}] ", self.labels.first().unwrap().span)
                } else {
                    "".to_string()
                },
                self.level.variant_name()
            )
            .color(match self.level {
                ReportLevel::Advice => AnsiColors::Blue,
                ReportLevel::Warn => AnsiColors::Yellow,
                ReportLevel::Error => AnsiColors::Red,
                ReportLevel::Silent => unreachable!(),
            }),
            self.title
        );
        if config.compact {
            return;
        }
        if !compact_span && !self.labels.is_empty() {
            write!(dst, "  ");
            if !config.compact && (self.help.is_some() || self.note.is_some()) {
                write!(dst, "{}", "╭─".bright_black());
            }
            writeln!(dst, "[{}] ", self.labels.first().unwrap().span);
        }
        if let Some(help) = self.help {
            writeln!(
                dst,
                "  {} {}: {}",
                "│".bright_black(),
                "Help".fg::<CustomColor<132, 209, 172>>(),
                help
            );
        }
        if let Some(note) = self.note {
            writeln!(
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
        let mut buffer: Vec<u8> = Vec::new();
        let config = ReportConfig::default();
        for report in self.receiver.try_iter() {
            if report.level == ReportLevel::Error {
                errors += 1;
            }
            if !Self::should_display(&*report) || self.reported == ARGS.max_reports {
                continue;
            }
            report.write(&mut buffer, config);
            self.reported += 1;
        }
        if errors > 0 {
            if ARGS.report_level != ReportLevel::Silent {
                eprintln!(
                    "{}{}",
                    std::str::from_utf8(&buffer).unwrap(),
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

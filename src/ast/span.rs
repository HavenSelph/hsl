#![allow(unused)]
use crate::report::UnwrapReport;
use std::fmt::{Debug, Display, Formatter};

pub struct Location {
    pub filename: &'static str,
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn at(filename: &'static str, index: usize) -> Self {
        let file = crate::files::get_source(filename).unwrap_reported();
        if file.is_empty() {
            return Self {
                filename,
                index,
                line: 1,
                column: 1,
            };
        }
        let line = file.line_at(index);
        let column = file.column_at(index);
        Self {
            filename,
            index,
            line,
            column,
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Copy, Clone)]
pub struct Span {
    pub filename: &'static str,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(filename: &'static str, start: usize, end: usize) -> Self {
        Self {
            filename,
            start,
            end,
        }
    }

    pub fn at(filename: &'static str, index: usize) -> Self {
        Self::new(filename, index, index + 1)
    }

    pub fn empty() -> Self {
        Self::new("", 0, 0)
    }

    pub fn extend(&self, other: Self) -> Self {
        Self {
            filename: self.filename,
            start: self.start,
            end: other.end,
        }
    }

    pub fn length(&self) -> usize {
        self.end - self.start
    }

    pub fn start_location(&self) -> Location {
        Location::at(self.filename, self.start)
    }

    pub fn end_location(&self) -> Location {
        Location::at(self.filename, self.end)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.filename, self.start_location())
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}-{}", self.end_location())
    }
}

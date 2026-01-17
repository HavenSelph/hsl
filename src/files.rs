use crate::report::{ReportKind, ReportLevel};
use dashmap::{DashMap, Entry};
use std::fs::File;
use std::io::{BufReader, Read, Result};
use std::sync::LazyLock;

struct InvalidFile(String);

impl ReportKind for InvalidFile {
    fn title(&self) -> String {
        format!("InvalidFile {:?}", self.0)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }
}

/// Cached source file contents
pub struct SourceFile {
    text: String,
    line_starts: Vec<usize>,
}

impl SourceFile {
    fn new(text: String) -> Self {
        let mut line_starts = vec![0];
        for (i, c) in text.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }
        Self { text, line_starts }
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    /// Get line number (1-indexed) for a byte offset
    pub fn line_at(&self, offset: usize) -> usize {
        match self.line_starts.binary_search(&offset) {
            Ok(line) => line + 1,
            Err(line) => line,
        }
    }

    /// Get column number (1-indexed) for a byte offset
    pub fn column_at(&self, offset: usize) -> usize {
        let line = self.line_at(offset);
        let line_start = self.line_starts.get(line - 1).copied().unwrap_or(0);
        offset - line_start + 1
    }

    /// Get the text of a specific line (1-indexed)
    pub fn get_line(&self, line: usize) -> Option<&str> {
        if line == 0 || line > self.line_starts.len() {
            return None;
        }
        let start = self.line_starts[line - 1];
        let end = self
            .line_starts
            .get(line)
            .copied()
            .unwrap_or(self.text.len());
        // Trim trailing newline if present
        let line_text = &self.text[start..end];
        Some(line_text.trim_end_matches('\n'))
    }
}

static CACHE: LazyLock<DashMap<&'static str, &'static SourceFile>> = LazyLock::new(|| {
    let hm: DashMap<&'static str, &'static SourceFile> = DashMap::with_capacity(2);
    hm.insert("", Box::leak(Box::new(SourceFile::new(String::new()))));
    hm
});

struct Scanner {
    filename: &'static str,
    contents: String,
    reader: BufReader<File>,
}

impl Scanner {
    fn new(filename: &'static str) -> Result<Self> {
        let file = File::open(filename)?;
        let file_size = file.metadata()?.len() as usize;

        Ok(Self {
            filename,
            contents: String::with_capacity(file_size),
            reader: BufReader::new(file),
        })
    }

    fn read(mut self) -> Result<Self> {
        let mut buf = [0u8; 1];

        while self.reader.read(&mut buf)? > 0 {
            match std::str::from_utf8(&buf) {
                Ok(s) => match s {
                    "\r" => continue,
                    _ => self.contents.push_str(s),
                },
                Err(_) => {
                    eprintln!("Failed to read from file: {}", self.filename);
                    std::process::exit(1);
                }
            }
        }
        Ok(self)
    }
}

pub fn get_source(filename: &'static str) -> crate::report::Maybe<&'static SourceFile> {
    match CACHE.entry(filename) {
        Entry::Occupied(entry) => Ok(*entry.get()),
        Entry::Vacant(entry) => {
            let contents = Scanner::new(filename)
                .map_err(|e| InvalidFile(filename.to_string()).make().with_note(e))?
                .read()
                .map_err(|e| InvalidFile(filename.to_string()).make().with_note(e))?
                .contents;
            let leaked: &'static SourceFile = Box::leak(Box::new(SourceFile::new(contents)));
            entry.insert(leaked);
            Ok(leaked)
        }
    }
}

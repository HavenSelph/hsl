use crate::report::{ReportKind, ReportLevel, UnwrapReport};
use ariadne::{Cache, Source};
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

static CACHE: LazyLock<DashMap<&'static str, &'static Source>> = LazyLock::new(|| {
    let hm = DashMap::with_capacity(2);
    hm.insert("", &*Box::leak(Source::from("".to_string()).into()));
    hm
});

pub struct ScannerCache {}
impl Cache<&'static str> for ScannerCache {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &&'static str,
    ) -> std::result::Result<&Source<<Self as Cache<&'static str>>::Storage>, impl std::fmt::Debug>
    {
        Ok::<&Source, ()>(get_source(id).unwrap_reported())
    }

    fn display<'a>(&self, id: &'a &'static str) -> Option<impl std::fmt::Display + 'a> {
        Some(Box::new(id))
    }
}

pub struct Scanner {
    filename: &'static str,
    index: usize,
    contents: String,
    reader: BufReader<File>,
}

impl Scanner {
    fn new(filename: &'static str) -> Result<Self> {
        let file = File::open(filename)?;
        let file_size = file.metadata()?.len() as usize;

        Ok(Self {
            filename,
            index: 0,
            contents: String::with_capacity(file_size),
            reader: BufReader::new(file),
        })
    }

    fn read(mut self) -> Result<Self> {
        let mut buf = [0u8; 1];

        while self.reader.read(&mut buf)? > 0 {
            match std::str::from_utf8(&buf) {
                Ok(s) => match s {
                    "\r" => {
                        continue;
                    }
                    _ => self.contents.push_str(s),
                },
                Err(_) => {
                    eprintln!("Failed to read from file: {}", self.filename);
                    std::process::exit(1);
                }
            }
            self.index += 1;
        }
        Ok(self)
    }
}

pub fn get_source(filename: &'static str) -> crate::report::Maybe<&'static Source> {
    match CACHE.entry(filename) {
        Entry::Occupied(entry) => Ok(entry.get()),
        Entry::Vacant(entry) => {
            let contents = Scanner::new(filename)
                .map_err(|e| InvalidFile(filename.to_string()).make().with_note(e))?
                .read()
                .map_err(|e| InvalidFile(filename.to_string()).make().with_note(e))?
                .contents;
            Ok(entry
                .insert(Box::leak(Source::from(contents).into()))
                .value_mut())
        }
    }
}

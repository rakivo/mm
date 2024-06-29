use std::{
    str::Lines,
    collections::HashMap,
    iter::{Enumerate, Peekable},
};
use crate::{Inst, Type, ALL_VARIANTS_AS_STR};

#[derive(Debug)]
pub enum Macro<'a> {
    MultiLine {
        row: usize,
        args_count: usize,
        args_names: Vec::<&'a str>,
        body: Vec::<Vec::<&'a str>>,
    },
    SingleLine {
        row: usize,
        body: String,
    }
}

pub type Macros<'a> = HashMap::<String, Macro<'a>>;

pub struct Comptime<'a> {
    file_path: &'a str,
    iter: Peekable<Enumerate::<Lines<'a>>>,
    macros: &'a mut Macros<'a>,
}

impl<'a> Comptime<'a> {
    pub fn new(content: &'a str, file_path: &'a str, macros: &'a mut Macros<'a>) -> Self {
        Self {
            file_path,
            iter: content.lines().enumerate().peekable(),
            macros,
        }
    }

    fn parse_macro(&mut self, line: &'a str, row: usize) {
        let trimmed = line.trim();
        if trimmed.ends_with('{')
            || matches!(self.iter.peek(), Some(line) if line.1.trim().starts_with('{'))
        {
            let splitted = trimmed.split_whitespace().collect::<Vec::<_>>();
            let name = {
                let first = splitted.first();
                if matches!(first, Some(s) if s.len() == 1 || !s.starts_with('#')) {
                    panic!("{f}:{r}: Macro without a name", f = self.file_path, r = row + 1)
                } else {
                    first.unwrap()[1..].to_owned()
                }
            };

            let args_names = splitted[1..].iter().take_while(|c| **c != "{").cloned().collect::<Vec::<_>>();
            let args_count = args_names.len();

            if !line.contains('{') {
                while let Some((_, l)) = self.iter.peek() {
                    if !l.trim().is_empty() { panic!("WTF") }
                    else if l.contains('{') { break }
                    else { self.iter.next() };
                }
            }

            let body = {
                let mut body = Vec::new();
                while let Some((r, l)) = self.iter.peek() {
                    if l.contains('}') { break }
                    else {
                        body.push(l.split_whitespace().collect::<Vec::<_>>());
                        self.iter.next();
                    }
                } body
            };

            for line in body.iter() {
                let _ = line.iter().any(|w| {
                    let ret = ALL_VARIANTS_AS_STR.contains(&w)
                                && args_names.contains(&w)
                                && self.macros.contains_key(w.to_owned());
                    if ret {
                        panic!("{f}:{r}: WTF is this scheiise", f = self.file_path, r = row + 1)
                    } ret
                });
            }

            let macr = Macro::MultiLine {
                row, args_count, args_names, body
            };

            self.macros.insert(name, macr);
        } else {
            let splitted = trimmed.split_whitespace().collect::<Vec::<_>>();

            let name = {
                let first = splitted.first();
                if matches!(first, Some(s) if s.len() == 1 || !s.starts_with('#')) {
                    panic!("{f}:{r}: Macro without a name", f = self.file_path, r = row + 1)
                } else {
                    first.unwrap()[1..].to_owned()
                }
            };

            let body = splitted[1..].join(" ");

            let macr = Macro::SingleLine {
                row, body
            };

            self.macros.insert(name, macr);
        }
    }

    fn parse_line(&mut self, line: &'a str, row: usize) {
        let Some(first) = line.chars().next() else { return };

        if first == '#' {
            self.parse_macro(line, row);
        }
    }

    pub fn parse_macros(&mut self) -> &mut Macros<'a> {
        while let Some((row, line)) = self.iter.next() {
            self.parse_line(line, row);
        };
        println!("{:#?}", self.macros);
        self.macros
    }
}

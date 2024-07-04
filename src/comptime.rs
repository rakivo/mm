use std::{
    str::Lines,
    collections::HashMap,
    iter::{Enumerate, Peekable},
};
use crate::Inst::ALL_VARIANTS_AS_STR;

#[derive(Debug)]
pub enum Macro<'a> {
    MultiLine {
        name: String,
        row: usize,
        args_count: usize,
        args_names: Vec::<&'a str>,
        body: Vec::<Vec::<&'a str>>,
    },
    SingleLine {
        name: String,
        row: usize,
        body: String,
    }
}

impl<'a> Macro<'a> {
    fn body_of_single(&self) -> Option::<&str> {
        match self {
            Macro::SingleLine { body, .. } => Some(body),
            _ => None
        }
    }

    pub fn expand(&self, call_line: &str, macros: &Macros) -> String {
        let mut ret = String::new();
        match self {
            Macro::SingleLine { name, body, .. } => {
                for w in call_line.split_whitespace() {
                    if w == name {
                        ret.push(' ');
                        ret.push_str(body);
                    } else {
                        ret.push_str(w);
                    }
                }
            }
            Macro::MultiLine { name, args_count, args_names, body, .. } => {
                let mut iter = call_line.split_whitespace();
                while let Some(w) = iter.next() {
                    if w == name {
                        if *args_count > 0 {
                            let wlen = w.len();
                            assert!(call_line.len() > wlen + 1, "Lack of expected arguments.");
                            let args = call_line.trim()[wlen + 1..].split_ascii_whitespace().collect::<Vec::<_>>();
                            let args_len = args.len();
                            assert!(args_len == *args_count, "Args counts does not match, expected: {args_count}, got: {args_len}");
                            let args_map = args_names.iter().zip(&args).collect::<HashMap::<_, _>>();
                            for line_ in body.iter() {
                                for w_ in line_ {
                                    if let Some(w_) = args_map.get(w_) {
                                        if !(ret.is_empty() || matches!(ret.chars().last(), Some(ch) if ch == ' ' || ch == '\n')) {
                                            ret.push(' ');
                                        }
                                        ret.push_str(w_);
                                        ret.push('\n');
                                    } else if let Some(w_macr) = macros.get(w_.to_owned()) {
                                        let body = w_macr.body_of_single().unwrap();
                                        ret.push(' ');
                                        ret.push_str(body);
                                    } else {
                                        if !(ret.is_empty() || matches!(ret.chars().last(), Some(ch) if ch == ' ' || ch == '\n')) {
                                            ret.push(' ');
                                        }
                                        ret.push_str(w_);
                                    }
                                }
                            }
                        } else {
                            ret.push_str(w);
                        }
                    }
                }
            }
        }
        ret
    }
}

pub type Macros<'a> = HashMap::<String, Macro<'a>>;

pub struct Comptime<'a> {
    file_path: &'a str,
    iter: Peekable<Enumerate::<Lines<'a>>>,
    pub macros: Macros<'a>,
}

impl<'a> Comptime<'a> {
    pub fn new(content: &'a str, file_path: &'a str) -> Self {
        Self {
            file_path,
            iter: content.lines().enumerate().peekable(),
            macros: Macros::new(),
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
                while let Some((_, l)) = self.iter.next() {
                    if !l.trim().is_empty() { panic!("WTF") }
                    else if l.contains('{') { break }
                }
            }

            let body = {
                let mut body = Vec::new();
                while let Some((_, l)) = self.iter.next() {
                    if l.contains('}') { break }
                    else { body.push(l.split_whitespace().collect::<Vec::<_>>()); }
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
                name: name.to_owned(), row, args_count, args_names, body
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
                name: name.to_owned(), row, body
            };

            self.macros.insert(name, macr);
        }
    }

    #[inline]
    fn parse_line(&mut self, line: &'a str, row: usize, ret: &mut Vec::<&'a str>) {
        let Some(first) = line.chars().next() else { return };

        if first == '#' {
            self.parse_macro(line, row);
        } else {
            ret.push(line);
        }
    }

    #[inline]
    pub fn parse_macros(&mut self) -> Vec::<&'a str> {
        let mut ret = Vec::new();
        while let Some((row, line)) = self.iter.next() {
            self.parse_line(line, row, &mut ret);
        };
        ret
    }
}

pub fn process_content<'a>(macros: &Macros<'a>, content: &Vec::<&'a str>) -> String {
    let mut ret = String::new();
    for line in content.iter() {
        if let Some(w) = line.split_whitespace().find(|x| macros.get(x.to_owned()).is_some()) {
            let macr = macros.get(w).unwrap();
            ret.push('\t');
            ret.push_str(&macr.expand(line, macros));
            ret.push('\n');
        } else {
            ret.push_str(line);
            ret.push('\n');
        }
    }

    ret
}

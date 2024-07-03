use std::{
    env,
    str::Lines,
    borrow::Cow,
    fmt::Display,
    process::exit,
    iter::{Enumerate, Peekable},
};

#[allow(unused)]
mod lexer {
    pub type Result<T> = std::result::Result<T, ()>;
    pub type Iterator<'a> = std::vec::IntoIter<(usize, &'a str)>;
}

pub type Loc = (usize, usize);
pub type Tokens<'a> = Vec::<Token<'a>>;
pub type Labels<'a> = Vec::<(Token<'a>, Tokens<'a>)>;

const QUOTE: &str = "\"";

#[derive(Debug, Clone, PartialEq)]
pub enum PpType<'a> {
    Include,
    SingleLine {
        value: Cow<'a, str>
    },
    MultiLine {
        body: Tokens<'a>,
        args: Tokens<'a>
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType<'a> {
    Float,
    Label,
    String,
    Integer,
    Literal,
    Pp(PpType<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    loc: Loc,
    typ: TokenType<'a>,
    val: Cow<'a, str>
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{r}:{c}: {t:?}:{v}", r = self.loc.0 + 1, c = self.loc.1 + 1, t = self.typ, v = self.val)
    }
}

impl<'a> Token<'a> {
    pub fn new(loc: Loc, typ: TokenType<'a>, val: std::borrow::Cow<'a, str>) -> Self {
        Self { loc, typ, val }
    }
}

pub struct Lexer<'a> {
    ts: Tokens<'a>,
    iter: Enumerate::<Peekable::<Lines<'a>>>,
    file_path: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn ts(&self) -> &Tokens {
        &self.ts
    }

    pub fn new(file_path: &'a str, content: &'a str) -> Self {
        Self {
            ts: Vec::new(),
            iter: content.lines().peekable().enumerate(),
            file_path,
        }
    }

    fn split_whitespace_preserve_indices(input: &str) -> lexer::Iterator {
        let (s, e, mut ret) = input.char_indices().fold((0, 0, Vec::new()),
            |(s, e, mut ret), (i, c)|
        {
            if c.is_whitespace() {
                if s != i {
                    ret.push((s, &input[s..i]));
                }
                (i + c.len_utf8(), e, ret)
            } else {
                (s, i + c.len_utf8(), ret)
            }
        });

        if s != e {
            ret.push((s, &input[s..]));
        }

        ret.into_iter()
    }

    pub fn collect_tokens_by_labels(&self) -> Labels {
        let (ts, mut ret, label) = self.ts.iter().fold((Vec::new(), Vec::new(), None),
            |(mut ts, mut ret, mut label), token|
        {
            match token.typ {
                TokenType::Label => {
                    if let Some(label) = label.take() {
                        ret.push((label, ts));
                    }
                    let t = Some(token.to_owned());
                    (Vec::new(), ret, t)
                }
                _ => if label.is_some() {
                    ts.push(token.to_owned());
                    (ts, ret, label)
                } else {
                    (ts, ret, label)
                }
            }
        });

        if let Some(label) = label { ret.push((label, ts)); }
        ret
    }

    #[inline(always)]
    fn is_at_start(x: &str) -> bool {
        matches!(x.chars().next(), Some(x) if x.is_ascii())
    }

    fn check_for_macros(&mut self, line: &'a str, row: usize) -> bool {
        if line.starts_with('#') {
            let splitted = line[1..].split_whitespace().collect::<Vec::<_>>();
            let typ = if splitted.len() == 2 {
                let value = splitted.get(1).map(|x| (*x).into()).expect("Expected value");
                TokenType::Pp(PpType::SingleLine { value })
            } else if matches!(line.chars().nth(1), Some(ch) if ch == '"') {
                TokenType::Pp(PpType::Include)
            } else {
                let args = Self::split_whitespace_preserve_indices(line)
                    .skip(1)
                    .take_while(|x| x.1 != "{")
                    .map(|(col, arg)| {
                        let loc = (row + 1, col);
                        assert!(Self::type_token(&arg) == TokenType::Literal, "Name of argument must be literal");
                        Token::new(loc, TokenType::Literal, arg.into())
                    }).collect::<Vec::<_>>();

                let mut body = Vec::new();
                while let Some((row, line)) = self.iter.next() {
                    if line.contains('}') { break }

                    let mut iter = Self::split_whitespace_preserve_indices(line);
                    while let Some((col, t)) = iter.next() {
                        let loc = (row, col);
                        let typ = Self::type_token(t);
                        let t = Token::new(loc, typ, t.into());
                        body.push(t);
                    }
                }

                TokenType::Pp(PpType::MultiLine { body, args })
            };

            let name = line[1..].split_whitespace()
                .skip_while(|x| *x == "#")
                .take(1)
                .collect::<Vec::<_>>()
                .join(" ")
                .into();

            let col = line.find(|x| x == '#').unwrap();
            let loc = (row, col);
            let t = Token::new(loc, typ, name);
            self.ts.push(t);
            true
        } else {
            false
        }
    }

    fn type_token(currt: &str) -> TokenType {
        if currt.starts_with(QUOTE) && currt.ends_with(QUOTE) {
            TokenType::String
        } else if Self::is_at_start(currt) && currt.ends_with(':') {
            TokenType::Label
        } else if currt.contains('.') {
            TokenType::Float
        } else if currt.parse::<u64>().is_ok() {
            TokenType::Integer
        } else {
            TokenType::Literal
        }
    }

    fn match_token(&mut self, currt: &'a str, currloc: Loc) {
        let typ = Self::type_token(currt);
        let t = if typ == TokenType::Label {
            Token::new(currloc, typ, currt[..currt.len() - 1].into())
        } else {
            Token::new(currloc, typ, currt.into())
        };
        self.ts.push(t);
    }

    fn lex_line(&mut self, line: &'a str, row: usize) {
        let mut iter = Self::split_whitespace_preserve_indices(&line).peekable();

        while let Some((col, t)) = iter.next() {
            self.match_token(t, (row, col))
        }
    }

    pub fn lex_file(&mut self) {
        while let Some((row, line)) = self.iter.next() {
            if !self.check_for_macros(line, row) {
                self.lex_line(line, row)
            }
        }
    }
}

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        eprintln!("Usage: {p} <input>", p = args[0]);
        exit(1)
    }

    let input = &args[1];
    let content = std::fs::read_to_string(input)?;
    let mut lexer = Lexer::new(input, &content);

    lexer.lex_file();
    for t in lexer.ts() {
        println!("{f}:{t}", f = input);
    }

    Ok(())
}

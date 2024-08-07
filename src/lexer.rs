use std::{
    str::Lines,
    fmt::Display,
    fs::read_to_string,
    collections::HashMap,
    iter::{Enumerate, Peekable},
};

use crate::get_truth;

mod lexer {
    pub type Iterator<'a> = std::vec::IntoIter::<(usize, &'a str)>;
}

pub type Loc = (usize, usize);
pub type Tokens = Vec::<Token>;
pub type ETokens = Vec::<EToken>;
pub type Labels = Vec::<(Token, Tokens)>;
pub type MacrosMap = HashMap::<String, Token>;
pub type Iterator = Peekable::<std::vec::IntoIter::<Token>>;
pub type CIterator<'a> = Enumerate::<Peekable::<Lines<'a>>>;

const QUOTE: &str = "\"";

#[derive(Debug, Clone)]
pub enum EToken {
    Token(Token),
    Expansion(String)
}

impl EToken {
    pub fn as_string(&self) -> String {
        match self {
            EToken::Expansion(s) => (*s).to_owned(),
            EToken::Token(t) => t.val.to_owned()
        }
    }
}

impl Display for EToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EToken::Token(t) => t.fmt(f),
            EToken::Expansion(val) => write!(f, "{val}")
        }
    }
}

#[derive(Eq, Hash, Debug, Clone, PartialEq)]
pub enum PpType {
    Include,
    SingleLine {
        value: String,
    },
    MultiLine {
        body: Tokens,
        args: Tokens
    }
}

#[derive(Eq, Hash, Debug, Clone, PartialEq)]
pub enum TokenType {
    Float,
    Label,
    String,
    Integer,
    Literal,
    Pp(PpType),
}

#[derive(Eq, Hash, Debug, Clone, PartialEq)]
pub struct Token {
    pub f: String,
    pub loc: Loc,
    pub typ: TokenType,
    pub val: String
}

impl Display for Token {
    #[inline]
    fn fmt(&self, f_: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f_, "{f}{r}:{c}: {t:?}:{v}", f = self.f, r = self.loc.0 + 1, c = self.loc.1 + 1, t = self.typ, v = self.val)
    }
}

impl Token {
    #[inline]
    pub fn new(f: String, loc: Loc, typ: TokenType, val: String) -> Self {
        Self { f, loc, typ, val }
    }
}

pub struct Lexer {
    ts: Tokens,
    mm: MacrosMap,
    #[allow(unused)]
    file_path: String,
}

impl<'a> Lexer {
    #[inline(always)]
    pub fn ts(&self) -> &Tokens {
        &self.ts
    }

    pub fn new(file_path: String) -> Self {
        Self {
            ts: Tokens::with_capacity(100),
            mm: MacrosMap::with_capacity(15),
            file_path,
        }
    }

    fn handle_include(s: String, mm: &mut MacrosMap, ets: &mut ETokens) {
        if s.ends_with(".masm") {
            if let Ok(content) = read_to_string(&s) {
                let lexer = Self::new(s.to_string());
                let mut iter = content.lines().peekable().enumerate();
                let (ets_, mut mm_) = lexer.lex_file(&mut iter);
                mm_.extend(mm.into_iter().map(|(a, b)| (a.to_owned(), b.to_owned())));
                *mm = mm_;
                ets.extend(ets_);
            } else {
                panic!("No such file: {f}", f = s)
            }
        } else {
            panic!("Unsupported extension: {e}", e = s.chars().rev().take_while(|x| x != &'.').collect::<std::string::String>().chars().rev().collect::<std::string::String>())
        }
    }

    fn match_token(iter: &mut Iterator, mm: &mut MacrosMap, t: Token, ets: &mut ETokens) {
        use { TokenType::*, PpType::* };

        match &t.typ {
            Pp(pp) => {
                match pp {
                    Include => Self::handle_include(t.val, mm, ets),
                    _ => {}
                }
            }
            _ => if let Some(m) = mm.get(&t.val) {
                match &m.typ {
                    Pp(pp) => match pp {
                        Include => unreachable!(),
                        SingleLine { value } => {
                            let value = get_truth(value.to_owned(), &mm);
                            if value.starts_with('#') {
                                if matches!(value.chars().nth(1), Some(ch) if ch == '"') {
                                    Self::handle_include(value[2..value.len() - 1].to_owned(), mm, ets)
                                } else {
                                    todo!()
                                }
                            } else {
                                ets.push(EToken::Expansion(value))
                            }
                        },
                        MultiLine { body, args } => {
                            let prev_row = t.loc.0;
                            let mut args_ = Vec::with_capacity(10);
                            while let Some(t) = iter.peek().cloned() {
                                if prev_row != t.loc.0 { break }
                                args_.push(t);
                                iter.next();
                            }

                            assert!(args_.len() == args.len(), "{f}:{ro}:{c}: Args count must be equal to the args count required in the macro, got: {g}, required: {r}, macro's name: {n}", f = t.f, ro = prev_row + 1, c = t.loc.1, g = args_.len(), r = args.len(), n = m.val);
                            let args_map = args.iter().map(|t| &t.val).zip(&args_).collect::<HashMap::<_, _>>();
                            for t_ in body.iter() {
                                if let Some(value) = args_map.get(&t_.val) {
                                    ets.push(EToken::Token((**value).to_owned()))
                                } else {
                                    ets.push(EToken::Token(t_.to_owned()));
                                }
                            }
                        }
                    }
                    _ => unreachable!()
                }
            } else {
                ets.push(EToken::Token(t))
            }
        }
    }

    fn expand_macros(mut self) -> (ETokens, MacrosMap) {
        let mut ets = ETokens::new();
        let mut iter = self.ts.into_iter().peekable();

        while let Some(t) = iter.next() {
            Self::match_token(&mut iter, &mut self.mm, t, &mut ets);
        }

        (ets, self.mm)
    }

    fn split_whitespace_preserve_indices(input: &str) -> lexer::Iterator {
        let (s, e, mut ret) = input.char_indices().fold((0, 0, Vec::with_capacity(input.len())),
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

    #[inline(always)]
    fn is_at_start(x: &str) -> bool {
        matches!(x.chars().next(), Some(x) if x.is_ascii())
    }

    fn check_for_macros(&mut self, line: &str, row: usize, iter: &mut CIterator) -> bool {
        if line.starts_with('#') {
            let splitted = line[1..].split_whitespace()
                .take_while(|x| x != &";")
                .collect::<Vec::<_>>();

            let typ = if splitted.len() == 2 && splitted[1] != "{" {
                let value = splitted.get(1).map(|x| (*x).into()).expect("Expected value");
                TokenType::Pp(PpType::SingleLine { value })
            } else if matches!(line.chars().nth(1), Some(ch) if ch == '"') {
                TokenType::Pp(PpType::Include)
            } else {
                let args = Self::split_whitespace_preserve_indices(line)
                    .skip(1)
                    .take_while(|x| x.1 != "{")
                    .map(|(col, arg)| {
                        let loc = (row, col);
                        assert!(Self::type_token(&arg) == TokenType::Literal, "Name of argument must be literal");
                        Token::new(self.file_path.to_owned(), loc, TokenType::Literal, arg.into())
                    }).collect::<Vec::<_>>();

                let mut body = Vec::with_capacity(150);
                while let Some((row, line)) = iter.next() {
                    let trimmed = line.trim();
                    if trimmed.starts_with(';') { continue }
                    if trimmed.contains('}') { break }

                    let mut iter = Self::split_whitespace_preserve_indices(trimmed).take_while(|x| x.1 != ";");
                    while let Some((col, t)) = iter.next() {
                        let loc = (row, col);
                        let typ = Self::type_token(t);
                        let t = Token::new(self.file_path.to_owned(), loc, typ, t.into());
                        body.push(t);
                    }
                }

                TokenType::Pp(PpType::MultiLine { body, args })
            };

            let name = line[1..].split_whitespace()
                .skip_while(|x| x == &"#")
                .take(1)
                .collect::<Vec::<_>>()
                .join(" ");

            let name = if matches!(typ, TokenType::Pp(ref pp) if matches!(pp, PpType::Include)) {
                name[1..name.len() - 1].to_owned()
            } else { name };

            let col = line.find(|x| x == '#').unwrap();
            let loc = (row, col);
            let t = Token::new(self.file_path.to_owned(), loc, typ, name.to_owned());
            self.mm.insert(name, t.to_owned());
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

    fn lex_line(&mut self, line: &'a str, row: usize) {
        let mut iter = Self::split_whitespace_preserve_indices(line).take_while(|x| x.1 != ";").peekable();
        while let Some((col, t)) = iter.next() {
            let loc = (row, col);
            let typ = Self::type_token(t);
            let token = if typ == TokenType::Label {
                Token::new(self.file_path.to_owned() ,loc, typ, t[..t.len() - 1].into())
            } else {
                Token::new(self.file_path.to_owned(), loc, typ, t.into())
            };
            self.ts.push(token);
        }
    }

    pub fn lex_file(mut self, iter: &mut CIterator) -> (ETokens, MacrosMap) {
        while let Some((row, line)) = iter.next() {
            let trimmed = line.trim();
            if trimmed.starts_with(';') || trimmed.is_empty() { continue }
            if !self.check_for_macros(line, row, iter) {
                self.lex_line(trimmed, row)
            }
        }

        self.expand_macros()
    }
}

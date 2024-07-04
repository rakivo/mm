use std::{
    borrow::Cow,
    collections::VecDeque, process::exit, time::Instant
};
use crate::{NaNBox, Inst, EToken, Flags, InstType, InstValue, Labels, Lexer, MTrap, Mm, Program, TokenType, Trap, ENTRY_POINT};

pub type MMResult<'a, T> = std::result::Result::<T, MTrap<'a>>;

fn comptime_labels_check<'a>(program: &Program, labels: &Labels, file_path: Cow<'a, str>) -> MMResult::<'a, ()> {
    use InstType::*;
    for ((row, col), inst) in program.iter() {
        match inst.typ {
              JE
            | JL
            | JG
            | JNGE
            | JNE
            | JNLE
            | JZ
            | JNZ
            | JMP
            | CALL => {
                let label = inst.val.string();
                if !labels.contains_key(label) {
                    let trap = Trap::InvalidLabel(label.to_owned(), "Not found in label map".to_owned());
                    return Err(MTrap(file_path, (*row, *col), trap))
                }
            }
            _ => {}
        }
    }

    Ok(())
}

impl Mm {
    pub fn try_from_masm(file_path: &str) -> Result::<Mm, MTrap>
    where
        Self: Sized
    {
        let content = std::fs::read_to_string(&file_path).map_err(|err| {
            eprintln!("Failed to open file: {file_path}: {err}");
            err
        }).unwrap_or_report();

        let time = Instant::now();

        let lexer = Lexer::new(&file_path, &content);
        let mut iter = lexer.lex_file().into_iter();

        let mut program = Vec::new();
        while let Some(et) = iter.next() {
            match et {
                EToken::Token(t) => {
                    match t.typ {
                        TokenType::Label => {
                            let inst = Inst {
                                typ: InstType::LABEL,
                                val: InstValue::String(t.val.into())
                            };
                            program.push((t.loc, inst))
                        }
                        TokenType::Literal => {
                            let Ok(typ) = InstType::try_from(&t.val) else {
                                panic!("SCHEISSEE: {file_path}:{r}:{c}: {v}", r = t.loc.0, c = t.loc.1, v = t.val)
                            };

                            let inst = if typ.is_arg_required() {
                                let arg = iter.next().unwrap();
                                let sarg = match arg {
                                    EToken::Expansion(s) => s,
                                    EToken::Token(t) => t.val
                                };

                                let val = match typ {
                                    InstType::PUSH | InstType::CMP | InstType::DUP => {
                                        InstValue::F64 (
                                            if sarg.contains('.') {
                                                let Ok(v) = sarg.parse::<f64>() else {
                                                    panic!("{file_path}:{r}:{c}: Invalid type, expected: u64 or f64", r = t.loc.0 + 1, c = t.loc.1)
                                                };
                                                NaNBox(v)
                                            } else {
                                                let Ok(v) = sarg.parse::<u64>() else {
                                                    panic!("{file_path}:{r}:{c}: Invalid type, expected: u64 or f64", r = t.loc.0 + 1, c = t.loc.1)
                                                };
                                                NaNBox::from_u64(v)
                                            }
                                        )
                                    },
                                    InstType::DMP => InstValue::U8(sarg.parse::<u8>().unwrap()),
                                    _ => InstValue::String(sarg.to_string())
                                };

                                Inst {
                                    typ,
                                    val
                                }
                            } else {
                                Inst::try_from(typ).unwrap()
                            };
                            program.push((t.loc, inst));
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        if matches!(program.last(), Some(last) if last.1 != Inst::HALT) {
            program.push(((program.last().unwrap().0.0 + 1, 69), Inst::HALT));
        }

        let labels = Mm::process_labels(&program);
        let Some(entry_point) = labels.to_owned().into_iter().find(|(l, _)| *l == ENTRY_POINT) else {
            let trap = Trap::NoEntryPointFound(file_path.to_owned());
            return Err(MTrap(file_path.into(), (0, 0), trap))
        };

        comptime_labels_check(&program, &labels, file_path.into()).unwrap_or_report();

        let elapsed = time.elapsed().as_micros();
        println!("Parsing and comptime checks took: {elapsed} microseconds");

        let mm = Mm {
            file_path: file_path.to_owned(),
            stack: VecDeque::with_capacity(Mm::STACK_CAP),
            call_stack: if program.is_empty() {
                VecDeque::with_capacity(Mm::CALL_STACK_CAP)
            } else {
                vec![program.len() - 1].into()
            },
            labels,
            flags: Flags::new(),
            program,
            ip: entry_point.1,
            halt: false
        };

        Ok(mm)
    }
}

pub trait UnwrapOrReport<T> {
    fn unwrap_or_report(self) -> T;
}

impl<T, E> UnwrapOrReport<T> for Result<T, E>
where
    E: std::fmt::Debug
{
    #[inline]
    #[track_caller]
    fn unwrap_or_report(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => {
                eprintln!("[ERROR] {e:?}");
                if cfg!(debug_assertions) {
                    panic!("called `Option::unwrap()` on a `None` value")
                } else {
                    exit(1)
                }
            }
        }
    }
}

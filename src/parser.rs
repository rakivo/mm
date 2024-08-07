use std::{
    borrow::Cow,
    process::exit,
    time::Instant,
    fs::read_to_string,
    collections::VecDeque,
};
use crate::{MEMORY_CAP, EToken, Externs, Flags, Inst, InstType, InstValue, Labels, Lexer, MTrap, MacrosMap, Mm, NaNBox, Natives, PpType, Program, Token, TokenType, Trap, DEBUG, ENTRY_POINT};

pub type MMResult<'a, T> = std::result::Result::<T, MTrap<'a>>;

fn comptime_labels_check<'a>(program: &'a Program, labels: &Labels, externs: &Externs, natives: &Natives, file_path: Cow<'a, str>) -> MMResult::<'a, ()> {
    use InstType::*;
    for ((row, col), inst) in program.iter() {
        match inst.typ {
              JE
            | JL
            | JG
            | JLE
            | JNE
            | JGE
            | JZ
            | JNZ
            | JMP
            | CALL => {
                let label = inst.val.as_string();
                if !labels.contains_key(label.as_str()) && !externs.contains_key(label.as_str()) && !natives.contains_key(label.as_str()) {
                    let trap = Trap::InvalidLabel(label, "CE: Not found in labels | natives | externs map");
                    return Err(MTrap(file_path, (*row, *col), trap))
                }
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn get_truth<'a>(s: String, map: &MacrosMap) -> String {
    if let Some(truth) = map.get(&s) {
        let TokenType::Pp(ref pp) = truth.typ else { return s };
        match pp {
            PpType::SingleLine { value } => get_truth(value.to_owned(), map),
            _ => todo!(),
        }
    } else {
        s
    }
}

impl<'a> Mm<'a> {
    pub fn try_from_masm(file_path: &'a str, _: Vec::<&'a str>, natives: Natives) -> Result::<(Mm<'a>, Program), MTrap<'a>>
    where
        Self: Sized
    {
        // , iter: &mut CIterator<'a>

        let content = read_to_string(&file_path).map_err(|err| {
            eprintln!("Failed to open file: {file_path}: {err}");
            err
        }).unwrap_or_report();

        let mut iter = content.lines().peekable().enumerate();

        let time = Instant::now();

        let lexer = Lexer::new(file_path.to_owned());
        let (ts, mm) = lexer.lex_file(&mut iter);
        if ts.is_empty() { exit(0) }

        // TODO: remove this scheisse
        let mut prev = Token::new("urmom".to_owned(), (69, 420), TokenType::Literal, "1024".to_owned());

        let mut program = Vec::with_capacity(ts.len() / 2);
        let mut iter = ts.into_iter();
        while let Some(et) = iter.next() {
            let EToken::Token(t) = et else { continue };
            match t.typ {
                TokenType::Label => {
                    let inst = Inst {
                        typ: InstType::LABEL,
                        val: InstValue::String(t.val.into())
                    };
                    program.push((t.loc, inst))
                }
                TokenType::Literal => {
                    let Ok(typ) = InstType::try_from_string(&t.val.to_string()) else {
                        let trap = Trap::UndefinedSymbol(prev.val);
                        return Err(MTrap(t.f.into(), t.loc, trap))
                    };

                    let inst = if typ.is_arg_required() {
                        let arg_ = iter.next().expect("Expected argument after instruction");
                        let arg = get_truth(arg_.as_string(), &mm);

                        let val = match typ {
                            InstType::PUSH => {
                                if arg.contains('.') {
                                    let Ok(v) = arg.parse::<f64>() else {
                                        let trap = Trap::InvalidPpType(arg, "u64 or f64");
                                        return Err(MTrap(file_path.into(), t.loc, trap))
                                    };
                                    InstValue::NaN(NaNBox(v))
                                } else {
                                    let Ok(v) = arg.parse::<i64>() else {
                                        let trap = Trap::InvalidPpType(arg, "u64, i64 or f64");
                                        return Err(MTrap(file_path.into(), t.loc, trap))
                                    };
                                    InstValue::NaN(NaNBox::from_i64(v))
                                }
                            }

                              InstType::DUP
                            | InstType::SWAP
                            | InstType::READ8
                            | InstType::WRITE8
                            | InstType::READ16
                            | InstType::WRITE16
                            | InstType::READ32
                            | InstType::WRITE32
                            | InstType::READ64
                            | InstType::WRITE64 => {
                                let Ok(v) = arg.parse::<u64>() else {
                                    let trap = Trap::InvalidPpType(arg, "u64 or f64");
                                    return Err(MTrap(file_path.into(), t.loc, trap))
                                };
                                InstValue::U64(v)
                            }
                            InstType::IDMP | InstType::FDMP => {
                                let Ok(v) = arg.parse::<u8>() else {
                                    let trap = Trap::InvalidPpType(arg, "u8");
                                    return Err(MTrap(file_path.into(), t.loc, trap))
                                };
                                InstValue::U8(v)
                            }
                            InstType::NATIVE => {
                                InstValue::String(arg)
                            }
                            InstType::EXTERN => {
                                let Ok(v) = iter.next().expect("Expected argument after extern | native").as_string().parse::<u64>() else {
                                    let trap = Trap::InvalidPpType(arg, "u64");
                                    return Err(MTrap(file_path.into(), t.loc, trap))
                                };
                                InstValue::StringU64(arg, v)
                            }
                            _ => InstValue::String(arg.to_string())
                        };

                        Inst {
                            typ,
                            val
                        }
                    } else {
                        Inst::try_from(typ).unwrap()
                    };
                    program.push((t.loc, inst));
                    prev = t;
                }
                _ => {
                    let trap = Trap::UndefinedSymbol(t.val);
                    return Err(MTrap(file_path.into(), t.loc, trap))
                }
            }
        }

        if matches!(program.last(), Some(last) if last.1 != Inst::HALT) {
            program.push(((program.last().unwrap().0.0 + 1, 69), Inst::HALT));
        }

        let labels = Mm::process_labels(&program, file_path);
        let Some(entry_point) = labels.iter().find(|(l, _)| l == &&ENTRY_POINT).map(|(_, i)| *i) else {
            let trap = Trap::NoEntryPointFound(file_path);
            return Err(MTrap(file_path.into(), (0, 0), trap))
        };

        // let libs = lib_paths.iter().map(|l| load_lib(l).unwrap()).collect::<Vec::<_>>();
        // let externs = Mm::process_externs(&program, &libs);
        let externs = Externs::new();
        Mm::check_natives(&program, &natives).unwrap();

        comptime_labels_check(&program, &labels, &externs, &natives, file_path.into()).unwrap_or_report();

        if DEBUG {
            let elapsed = time.elapsed().as_micros();
            println!("Parsing and comptime checks took: {elapsed}μs");
        }

        let mm = Mm {
            file_path,
            stack: VecDeque::with_capacity(1024),
            call_stack: VecDeque::with_capacity(Mm::CALL_STACK_CAP),
            memory: [0; MEMORY_CAP],
            natives,
            externs,
            labels,
            flags: Flags::new(),
            ip: entry_point,
            halt: false
        };

        Ok((mm, program))
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

use std::{
    borrow::{Borrow, Cow},
    collections::VecDeque, iter::Enumerate, process::exit, str::Lines, time::Instant
};
use crate::{Inst, EToken, Flags, InstType, Labels, Lexer, MResult, MTrap, Mm, Program, TokenType, Trap, ENTRY_POINT};

pub type MMResult<'a, T> = std::result::Result::<T, MTrap<'a>>;

// Adding 1 to the row to convert it from 0-based indexing
impl std::fmt::Debug for MTrap<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (file_path, loc, trap) = (&self.0, &self.1, &self.2);
        let (row, col) = (loc.0, loc.1);
        write!(f, "{file_path}:{row}:{col}: {trap:?}")
    }
}

fn comptime_labels_check<'a>(program: &Program, labels: &Labels, file_path: Cow<'a, str>) -> MMResult::<'a, ()> {
    use InstType::*;
    for (inst, (row, col)) in program.iter() {
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
        let tokens = lexer.lex_file();
        for (i, et) in tokens.into_iter().enumerate() {
            println!("{et}");
            match et {
                EToken::Token(t) => {
                    match t.typ {
                        TokenType::Literal => {
                            let Ok(inst) = InstType::try_from(&t.val) else {
                                panic!("SCHEIISEE: {v}", v = t.val)
                            };
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        let elapsed = time.elapsed().as_micros();
        println!("Parsing and comptime checks took: {elapsed} microseconds");

        // let mm = Mm {
        //     file_path: file_path.to_owned(),
        //     stack: VecDeque::with_capacity(Mm::STACK_CAP),
        //     call_stack: if program.is_empty() {
        //         VecDeque::with_capacity(Mm::CALL_STACK_CAP)
        //     } else {
        //         vec![program.len() - 1].into()
        //     },
        //     labels,
        //     flags: Flags::new(),
        //     program,
        //     ip: entry_point.1,
        //     halt: false
        // };

        Ok(Mm::new(Program::new(), "ewq"))
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

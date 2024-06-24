use std::process::exit;
use crate::{Mm, Inst, Trap, Program, MResult, Flags, DEBUG};

#[derive(Debug)]
pub struct Info<'a>(&'a str, usize);

pub struct MTrap<'a>(Trap, Info<'a>);

impl std::fmt::Debug for MTrap<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (trap, info) = (&self.0, &self.1);
        let (file_path, row) = (info.0, info.1);
        write!(f, "{file_path}:{row}: {trap:?}")
    }
}

type MProgram = Vec::<MResult::<Inst>>;

fn parse_labels(line: &str) -> MProgram {
// label1, label2:
//     ...

    line.split(',')
        .map(|label| Ok(Inst::LABEL(label.trim().to_owned())))
        .collect()
}

#[inline]
fn parse_line(line: &str) -> MProgram {
    if line.ends_with(':') && matches!(line.chars().next(), Some(ch) if ch.is_ascii()) {
        parse_labels(&line[..line.len() - 1])
    } else {
        vec![Inst::try_from(line)]
    }
}

impl Mm {
    pub fn try_from_masm(file_path: &str) -> Result::<Mm, MTrap>
    where
        Self: Sized
    {
        let file = std::fs::read_to_string(&file_path).map_err(|err| {
            eprintln!("Failed to open file: {file_path}: {err}");
            err
        }).unwrap_or_report();

        let mut program = Program::new();
        for (row, line) in file.lines().filter(|l| !l.starts_with(';')).enumerate() {
            if line.trim().is_empty() { continue }
            program.extend
            (
                parse_line(&line).into_iter()
                    .map(|res| {
                        res.map_err(|err| MTrap(err, Info(file_path, row + 1)))
                           .unwrap_or_report()
                    })
            )
        }

        if matches!(program.last(), Some(last) if *last != Inst::HALT) {
            program.push(Inst::HALT);
        }

        let labels = Mm::process_labels(&program);

        if DEBUG {
            println!("{labels:?}");
            println!("{program:?}");
        }

        let mm = Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
            labels,
            flags: Flags::new(),
            program,
            ip: 0,
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

/* TODO:
    Compile time checks for invalid labels
*/

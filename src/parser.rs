use std::process::exit;
use crate::{Mm, Inst, Trap, MResult, Labels, Flags, DEBUG};

#[derive(Debug)]
pub struct Info<'a>(&'a str, usize);

pub struct MTrap<'a>(Trap, Info<'a>);

pub type MProgram = Vec::<(Inst, usize)>;
pub type MMResult<'a, T> = std::result::Result::<T, MTrap<'a>>;

// Adding 1 to the row to convert it from 0-based indexing
impl std::fmt::Debug for MTrap<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (trap, info) = (&self.0, &self.1);
        let (file_path, row) = (info.0, info.1 + 1);
        write!(f, "{file_path}:{row}: {trap:?}")
    }
}

fn parse_labels(line: &str, program: &mut MProgram, row: usize, _file_path: &str) -> MResult::<()> {
// label1, label2:
//     ...

    program.extend
    (
        line.split(',')
            .map(|label| (Inst::LABEL(label.trim().to_owned()), row))
    );

    Ok(())
}

fn parse_line(line: &str, program: &mut MProgram, row: usize, file_path: &str) -> MResult::<()> {
    if line.ends_with(':') && matches!(line.chars().next(), Some(ch) if ch.is_ascii()) {
        parse_labels(&line[..line.len() - 1], program, row, file_path)
    } else {
        program.push((Inst::try_from(line).map_err(|err| {
            MTrap(err, Info(file_path, row))
        }).unwrap_or_report(), row));

        Ok(())
    }
}

fn comptime_jumps_check<'a>(program: &MProgram, labels: &Labels, file_path: &'a str) -> MMResult::<'a, ()> {
    use Inst::*;
    for (inst, row) in program.iter() {
        match inst {
              JE(ref label)
            | JL(ref label)
            | JG(ref label)
            | JNGE(ref label)
            | JNE(ref label)
            | JNLE(ref label)
            | JZ(ref label)
            | JNZ(ref label)
            | JMP(ref label) =>
            {
                if !labels.contains_key(label) {
                    let trap = Trap::InvalidLabel(label.to_owned(), "Not found in label map".to_owned());
                    let info = Info(file_path, *row);
                    return Err(MTrap(trap, info))
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
        let file = std::fs::read_to_string(&file_path).map_err(|err| {
            eprintln!("Failed to open file: {file_path}: {err}");
            err
        }).unwrap_or_report();

        let mut program = MProgram::new();
        for (row, line) in file.lines().enumerate() {
            if line.trim().is_empty() || line.starts_with(';') { continue }
            parse_line(&line, &mut program, row, file_path).unwrap_or_report();
        }

        if matches!(program.last(), Some(last) if last.0 != Inst::HALT) {
            program.push((Inst::HALT, program.last().unwrap().1 + 1));
        }

        let labels = Mm::process_labels_m(&program);

        if DEBUG {
            println!("{labels:?}");
            println!("{program:?}");
        }

        comptime_jumps_check(&program, &labels, file_path).unwrap_or_report();

        let mm = Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
            labels,
            flags: Flags::new(),
            program: program.into_iter().map(|x| x.0).collect(),
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

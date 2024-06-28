use std::{
    process::exit,
    collections::VecDeque,
    time::{SystemTime, UNIX_EPOCH, Duration}
};
use crate::{Mm, Inst, Trap, MResult, Labels, Flags, Funcs, DEBUG, ENTRY_POINT_FUNCTION};

#[derive(Debug)]
pub struct Info<'a>(&'a str, usize);

pub struct MTrap<'a>(Trap, Option::<Info<'a>>);

pub type MProgram = Vec::<(Inst, usize)>;
pub type MMResult<'a, T> = std::result::Result::<T, MTrap<'a>>;

// Adding 1 to the row to convert it from 0-based indexing
impl std::fmt::Debug for MTrap<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (trap, info) = (&self.0, &self.1);
        if let Some(info) = info {
            let (file_path, row) = (info.0, info.1 + 1);
            write!(f, "{file_path}:{row}: {trap:?}")
        } else {
            write!(f, "{trap:?}")
        }
    }
}

pub fn time_msg(msg: &str) {
    let now = SystemTime::now();
    if let Ok(dur) = now.duration_since(UNIX_EPOCH) {
        let secs_ = dur.as_secs();
        let nans_ = dur.subsec_nanos();

        let dt_secs = (UNIX_EPOCH + Duration::new(secs_, nans_)).duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_secs();

        let rem_secs = dt_secs % 86400;
        let hrs = rem_secs / 3600;
        let rem_secs = rem_secs % 3600;
        let mins = rem_secs / 60;
        let secs = rem_secs % 60;

        println!("{msg} at: {hrs:02}:{mins:02}:{secs:02}")
    }
}

fn parse_labels(line: &str, program: &mut MProgram, row: usize, _file_path: &str) -> MResult::<()> {
// label1, label2:
//     ...

    let splitted = line.split_whitespace().collect::<Vec::<_>>();
    assert!(splitted.len() == 1, "Scheisse is NOT allowed here");

    program.extend
    (
        line[..line.len() - 1].split(',')
            .map(|label| (Inst::LABEL(label.trim().to_owned()), row))
    );

    Ok(())
}

fn parse_function(line: &str, program: &mut MProgram, row: usize, _file_path: &str) -> MResult::<()> {
// func ::
//     ...

    let splitted = line.split_whitespace().collect::<Vec::<_>>();
    assert!(splitted.len() <= 2, "Scheisse is NOT allowed here");

    program.extend
    (
        line[..line.len() - 2].split(',')
            .map(|func| (Inst::FUNC(func.trim().to_owned()), row))
    );

    Ok(())
}

fn parse_line(line: &str, program: &mut MProgram, row: usize, file_path: &str) -> MResult::<()> {
    let first = line.chars().next();
    let matches = matches!(&first, Some(ch) if ch.is_ascii());

    if matches && line.ends_with("::") {
        assert!(line.len() > 2, "Function without a name");
        parse_function(&line, program, row, file_path)
    } else if matches && line.ends_with(':') {
        assert!(line.len() > 1, "Label without a name");
        parse_labels(&line, program, row, file_path)
    } else {
        let inst = Inst::try_from(line).map_err(|err| {
            MTrap(err, Some(Info(file_path, row)))
        }).unwrap_or_report();

        program.push((inst, row));

        Ok(())
    }
}

fn comptime_jfs_check<'a>(program: &MProgram, labels: &Labels, funcs: &Funcs, file_path: &'a str) -> MMResult::<'a, ()> {
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
                    return Err(MTrap(trap, Some(info)))
                }
            }
            CALL(ref func) => {
                if !funcs.contains_key(func) {
                    let trap = Trap::InvalidFunction(func.to_owned(), "Not found in function map".to_owned());
                    let info = Info(file_path, *row);
                    return Err(MTrap(trap, Some(info)))
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

        time_msg("Started parsing");

        let mut program = MProgram::new();
        for (row, line) in file.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with(';') { continue }
            parse_line(&line, &mut program, row, file_path).unwrap_or_report();
        }

        if matches!(program.last(), Some(last) if last.0 != Inst::HALT) {
            program.push((Inst::HALT, program.last().unwrap().1 + 1));
        }

        let funcs = Mm::process_funcs_m(&program);
        let labels = Mm::process_labels_m(&program);

        if DEBUG {
            println!("{labels:?}");
            println!("{program:?}");
        }

        let Some(entry_function) = funcs.to_owned().into_iter().find(|(l, _)| *l == ENTRY_POINT_FUNCTION) else {
            let trap = Trap::NoEntryPointFound(file_path.to_owned());
            return Err(MTrap(trap, None))
        };

        comptime_jfs_check(&program, &labels, &funcs, file_path).unwrap_or_report();

        time_msg("Ended parsing");

        let mm = Mm {
            stack: VecDeque::with_capacity(Mm::STACK_CAP),
            call_stack: VecDeque::with_capacity(Mm::STACK_CAP),
            funcs,
            labels,
            flags: Flags::new(),
            program: program.into_iter().map(|x| x.0).collect(),
            ip: entry_function.1,
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

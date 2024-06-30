use std::{
    str::Lines,
    process::exit,
    iter::Enumerate,
    collections::VecDeque,
    time::{SystemTime, UNIX_EPOCH, Duration}
};
use crate::{process_content, Comptime, Flags, Inst, Labels, MResult, MTrap, Mm, Program, Trap, DEBUG, ENTRY_POINT};

pub type MMResult<T> = std::result::Result::<T, MTrap>;

// Adding 1 to the row to convert it from 0-based indexing
impl std::fmt::Debug for MTrap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (trap, info) = (&self.0, &self.1);
        if let Some(info) = info {
            let (file_path, row) = (info.0.to_owned(), info.1 + 1);
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

struct Parser<'a> {
    iter: Enumerate::<Lines<'a>>,
    program: Program,
    file_path: &'a str
}

impl<'a> Parser<'a> {
    pub fn new(content: &'a str, file_path: &'a str) -> Self {
        Self {
            iter: content.lines().enumerate(),
            program: Program::new(),
            file_path
        }
    }

    fn parse_labels(&mut self, line: &'a str, row: usize) -> MResult::<()> {
        let splitted = line.split_whitespace().collect::<Vec::<_>>();
        assert!(splitted.len() == 1, "{f}:{r}: Scheisse is NOT allowed here", f = self.file_path, r = row + 1);

        self.program.extend
        (
            line[..line.len() - 1].split(',')
                .map(|label| (Inst::LABEL(label.trim().to_owned()), row))
        );

        Ok(())
    }

    fn parse_line(&mut self, line: &'a str, row: usize) -> MResult::<()> {
        let Some(first) = line.chars().next() else {
            return Ok(())
        };

        if first.is_ascii() && line.ends_with(':') {
            assert!(line.len() > 1, "Label without a name");
            self.parse_labels(&line, row)
        } else {
            let inst = Inst::try_from(line).map_err(|err| {
                MTrap(err, Some((self.file_path.to_owned(), row)))
            }).unwrap_or_report();

            self.program.push((inst, row));
            Ok(())
        }
    }

    fn parse(mut self) -> MResult::<Program> {
        if DEBUG {
            time_msg("Started parsing");
        }

        while let Some((row, line)) = self.iter.next() {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with(';') { continue }
            self.parse_line(&line, row)?;
        }

        if matches!(self.program.last(), Some(last) if last.0 != Inst::HALT) {
            self.program.push((Inst::HALT, self.program.last().unwrap().1 + 1));
        }

        if DEBUG {
            time_msg("Ended parsing");
        }

        Ok(self.program)
    }
}

fn comptime_labels_check<'a>(program: &Program, labels: &Labels, file_path: &'a str) -> MMResult::<()> {
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
            | JMP(ref label)
            | CALL(ref label) =>
            {
                if !labels.contains_key(label) {
                    let trap = Trap::InvalidLabel(label.to_owned(), "Not found in label map".to_owned());
                    return Err(MTrap(trap, Some((file_path.to_owned(), *row))))
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

        let mut ct = Comptime::new(&content, file_path);
        let content = ct.parse_macros();

        let macros = ct.macros;

        let content = process_content(&macros, &content);

        let parser = Parser::new(&content, file_path);

        let program = parser.parse().unwrap_or_report();
        let labels = Mm::process_labels(&program);

        let Some(entry_point) = labels.to_owned().into_iter().find(|(l, _)| *l == ENTRY_POINT) else {
            let trap = Trap::NoEntryPointFound(file_path.to_owned());
            return Err(MTrap(trap, None))
        };

        comptime_labels_check(&program, &labels, file_path).unwrap_or_report();

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

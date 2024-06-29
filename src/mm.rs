use std::collections::VecDeque;

pub mod nan;
pub mod flag;
pub mod inst;
pub mod trap;
pub mod parser;

pub use nan::*;
pub use flag::*;
pub use inst::*;
pub use trap::*;
pub use parser::*;

const DEBUG: bool = false;

const ENTRY_POINT: &str = "_start";

pub type Word = NaNBox;
pub type MResult<T> = std::result::Result<T, Trap>;

pub type Program = Vec<(Inst, usize)>;

pub type Labels = std::collections::HashMap<String, usize>;

pub struct Mm {
    pub file_path: String,

    stack: VecDeque::<Word>,
    call_stack: VecDeque::<usize>,

    labels: Labels,
    flags: Flags,
    program: Program,
    ip: usize,
    halt: bool,
}

#[inline]
fn print_oper_f(f: &mut std::fmt::Formatter<'_>, oper: &Word) -> std::fmt::Result {
    match oper.get_type().unwrap() {
        Type::F64 => write!(f, "{f}", f = oper.as_f64()),
        Type::I64 => write!(f, "{f}", f = oper.as_i64()),
        Type::U64 => write!(f, "{f}", f = oper.as_u64()),
        _ => todo!()
    }
}

#[inline]
fn print_oper_s<S>(mut s: S, oper: &Word) -> std::io::Result::<()>
where
    S: std::io::Write
{
    match oper.get_type().unwrap() {
        Type::F64 => write!(s, "{f}", f = oper.as_f64()),
        Type::I64 => write!(s, "{f}", f = oper.as_i64()),
        Type::U64 => write!(s, "{f}", f = oper.as_u64()),
        _ => todo!()
    }
}

impl std::fmt::Debug for Mm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack size: {size}\n", size = self.stack.len())?;
        write!(f, "stack:")?;
        let mut i = 0;
        while i < self.stack.len() {
            let oper = self.stack[i];
            write!(f, ", ")?;
            print_oper_f(f, &oper)?;
            i += 1;
        }
        Ok(())
    }
}

impl std::fmt::Display for Mm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack: ")?;
        if let Some(first) = self.stack.front() {
            print_oper_f(f, &first)?;
            let (mut i, n) = (1, self.stack.len());
            while i < n {
                let oper = self.stack[i];
                write!(f, ", ")?;
                print_oper_f(f, &oper)?;
                i += 1;
            }
        }
        Ok(())
    }
}

impl Mm {
    const STACK_CAP: usize = 8 * 1024;
    const CALL_STACK_CAP: usize = 8 * 128;

    fn process_labels(program: &Program) -> Labels {
        program.iter().fold((Labels::new(), 0), |(mut labels, ip), (inst, _)| {
            match inst {
                Inst::LABEL(label) => { labels.insert(label.to_owned(), ip); }
                _ => {}
            }
            (labels, ip + 1)
        }).0
    }

    pub fn new(program: Program, file_path: &str) -> Mm {
        Mm {
            file_path: file_path.to_owned(),
            stack: VecDeque::with_capacity(Mm::STACK_CAP),
            call_stack: if program.is_empty() {
                VecDeque::with_capacity(Mm::CALL_STACK_CAP)
            } else {
                vec![program.len() - 1].into()
            },
            labels: Self::process_labels(&program),
            flags: Flags::new(),
            program,
            ip: 0,
            halt: false,
        }
    }

    #[inline(always)]
    pub fn halt(&self) -> &bool {
        &self.halt
    }

    fn two_opers_finst(&mut self, inst: Inst, last: Word) -> MResult<()> {
        assert!(self.stack.len() > 0);

        let prelast = self.stack.back_mut().unwrap();
        let Some(b) = last.get_f64() else {
            return Err(Trap::DivisionOfDifferentTypes(prelast.get_type(), last.get_type()))
        };

        use Inst::*;
        match inst {
            FADD => prelast.0 += b,
            FSUB => prelast.0 -= b,
            FMUL => prelast.0 *= b,
            FDIV => prelast.0 /= b,
            _ => unreachable!(),
        }

        Ok(())
    }

    fn two_opers_iinst(&mut self, inst: Inst, last: Word) -> MResult<()> {
        assert!(self.stack.len() > 0);

        let prelast = self.stack.back_mut().unwrap();
        let (Some(a), Some(b)) = (prelast.get_u64(), last.get_u64()) else {
            return Err(Trap::DivisionOfDifferentTypes(prelast.get_type(), last.get_type()))
        };

        use Inst::*;
        match inst {
            IADD => { *prelast = Word::from_u64(a + b); }
            ISUB => { *prelast = Word::from_u64(a - b); }
            IMUL => { *prelast = Word::from_u64(a * b); }
            IDIV => {
                if b != 0 {
                    *prelast = Word::from_u64(a / b);
                } else {
                    return Err(Trap::DivisionByZero(inst))
                }
            }
            _ => unreachable!(),
        }

        *prelast = NaNBox(NaNBox::set_type(prelast.0, Type::U64));

        Ok(())
    }

    fn two_opers_inst(&mut self, inst: Inst, pop: bool, typeflag: u8) -> MResult<()> {
        let stack_len = self.stack.len();
        if stack_len < 2 {
            eprintln!("ERROR: Not enough operands on the stack, needed: 2, have: {stack_len}");
            eprintln!("Last executed instruction: {inst:?}");
            return Err(Trap::StackUnderflow(inst))
        }

        let last = if pop {
            self.stack.pop_back().unwrap()
        } else {
            self.stack[stack_len - 1].to_owned()
        };

        let prelast = &mut self.stack[stack_len - 2];

        use Inst::*;
        match inst {
            SWAP => {
                let a = *prelast;
                let b = last;
                self.stack.pop_back();
                self.stack.push_back(b);
                self.stack.push_back(a)
            }
            _ => match typeflag {
                1 => self.two_opers_iinst(inst, last)?,
                2 => self.two_opers_finst(inst, last)?,
                _ => unreachable!(),
            },
        }

        self.ip += 1;
        Ok(())
    }

    fn jump_if_flag(&mut self, label: &str, flag: Flag) -> Result<(), Trap> {
        let program_len = self.program.len();
        let Some(ip) = self.labels.get(&label.to_owned()) else {
            return Err(Trap::InvalidLabel(label.to_owned(), "Not found in label map".to_owned()))
        };

        if *ip >= program_len {
            eprintln!("ERROR: operand `{ip}` is outside of program bounds, program len: {program_len}");
            return Err(Trap::InvalidLabel(label.to_owned(), "Out of bounds".to_owned()))
        }

        if self.flags.is(flag) {
            self.ip = *ip;
        } else {
            self.ip += 1;
        }

        Ok(())
    }

    fn execute_instruction(&mut self, inst: Inst) -> Result<(), Trap> {
        if DEBUG {
            println!("{ip}: {inst}", ip = self.ip);
        }

        use Inst::*;
        match inst {
            NOP => Ok(()),

            PUSH(oper) => {
                if self.stack.len() < Mm::STACK_CAP {
                    self.stack.push_back(oper);
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackOverflow(inst.to_owned()))
                }
            }

            POP => {
                if !self.stack.is_empty() {
                    self.stack.pop_back();
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.to_owned()))
                }
            }

            INC => {
                if let Some(last) = self.stack.back_mut() {
                    let v = last.get_value();
                    *last = NaNBox::from_i64(v + 1);
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.to_owned()))
                }
            }

            DEC => {
                if let Some(last) = self.stack.back_mut() {
                    let v = last.get_value();
                    *last = NaNBox::from_i64(v - 1);
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.to_owned()))
                }
            }

            SWAP => self.two_opers_inst(SWAP, false, 0),

            IADD => self.two_opers_inst(IADD, true, 1),
            ISUB => self.two_opers_inst(ISUB, true, 1),
            IMUL => self.two_opers_inst(IMUL, true, 1),
            IDIV => self.two_opers_inst(IDIV, true, 1),

            FADD => self.two_opers_inst(FADD, true, 2),
            FSUB => self.two_opers_inst(FSUB, true, 2),
            FMUL => self.two_opers_inst(FMUL, true, 2),
            FDIV => self.two_opers_inst(FDIV, true, 2),

            CMP(oper) => {
                if let Some(ref last) = self.stack.back() {
                    self.flags.cmp(&last.as_u64(), &oper.as_u64());
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.to_owned()))
                }
            }

            DUP(oper) => {
                if self.stack.len() > oper.0 as usize {
                    if self.stack.len() < Mm::STACK_CAP {
                        let val = self.stack[self.stack.len() - 1 - oper.0 as usize];
                        self.stack.push_back(val);
                        self.ip += 1;
                        Ok(())
                    } else {
                        Err(Trap::StackOverflow(inst.to_owned()))
                    }
                } else {
                    Err(Trap::StackUnderflow(inst.to_owned()))
                }
            }

              JE(ref label)
            | JL(ref label)
            | JG(ref label)
            | JNGE(ref label)
            | JNE(ref label)
            | JNLE(ref label)
            | JZ(ref label)
            | JNZ(ref label) => self.jump_if_flag(label, Flag::try_from(&inst).unwrap()),

            JMP(label) => {
                let Some(ip) = self.labels.get(&label) else {
                    return Err(Trap::InvalidLabel(label, "Not found in label map".to_owned()))
                };

                if *ip < self.program.len() {
                    self.ip = *ip;
                    Ok(())
                } else {
                    eprintln!("ERROR: operand `{ip}` is outside of program bounds, program len: {len}",
                              len = self.program.len());
                    Err(Trap::InvalidLabel(label, "Out of bounds".to_owned()))
                }
            }

            BOT => {
                if let Some(first) = self.stack.front() {
                    if self.stack.len() < Mm::STACK_CAP {
                        self.stack.push_back(*first);
                        self.ip += 1;
                        Ok(())
                    } else {
                        Err(Trap::StackOverflow(inst))
                    }
                } else {
                    Err(Trap::StackUnderflow(inst))
                }
            }

            DMP(stream) => if let Some(last) = self.stack.back() {
                match stream {
                    1 => print_oper_s(std::io::stdout(), last).unwrap(),
                    2 => print_oper_s(std::io::stderr(), last).unwrap(),
                    _ => return Err(Trap::InvalidOperand(inst.to_string(), Some(stream.to_string())))
                }
                self.ip += 1;
                println!();
                Ok(())
            } else {
                Err(Trap::StackUnderflow(inst))
            }

            CALL(ref addr) => {
                if self.program.len() > self.ip {
                    if self.call_stack.len() < Self::CALL_STACK_CAP {
                        self.call_stack.push_back(self.ip + 1);
                    } else {
                        return Err(Trap::CallStackOverflow(inst.to_owned()))
                    }
                }

                if let Some(ip) = self.labels.get(addr) {
                    self.ip = *ip;
                    Ok(())
                } else {
                    Err(Trap::InvalidFunction(addr.to_owned(), "Not found in function map".to_owned()))
                }
            }

            RET => {
                if let Some(ip) = self.call_stack.pop_back() {
                    self.ip = ip;
                    Ok(())
                } else {
                    Err(Trap::CallStackUnderflow(inst))
                }
            }

            HALT => {
                self.halt = true;
                Ok(())
            }

            _ => {
                self.ip += 1;
                Ok(())
            }
        }
    }

    pub fn execute_program(&mut self, debug: bool, limit: Option::<usize>) -> MMResult<()> {
        if DEBUG {
            time_msg("Started executing program");
        }

        let mut count = 0;
        let limit = limit.unwrap_or(usize::MAX);
        while !self.halt() && count < limit {
            let (inst, row) = self.program[self.ip].to_owned();
            self.execute_instruction(inst).map_err(|trap| {
                MTrap::from((self.file_path.as_str(), row, trap))
            })?;

            if debug {
                println!("{self}");
            }

            count += 1;
        }

        if DEBUG {
            time_msg("Ended executing program");
        }

        Ok(())
    }

    pub fn to_binary(&self, file_path: &str) -> std::io::Result<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        for (inst, _) in self.program.iter() {
            f.write_all(&inst.as_bytes())?;
        }

        Ok(())
    }

    pub fn from_binary(file_path: &str) -> MResult<Mm> {
        use std::fs::read;

        let buf = read(file_path).map_err(|err| {
            eprintln!("Failed to read file: {file_path}: {err}");
            err
        }).unwrap();

        let (mut i, mut ip, mut program, mut labels) = (0, 0, Program::new(), Labels::new());
        while i < buf.len() {
            let (inst, size) = Inst::from_bytes(&buf[i..])?;
            match inst {
                Inst::LABEL(ref label) => { labels.insert(label.to_owned(), ip); }
                _ => {}
            };
            program.push((inst, ip));
            ip += 1;
            i += size;
        }

        if matches!(program.last(), Some(last) if last.0 != Inst::HALT) {
            program.push((Inst::HALT, ip + 1));
        }

        let mm = Mm {
            file_path: file_path.to_owned(),
            stack: VecDeque::with_capacity(Mm::STACK_CAP),
            call_stack: if let Some(last) = program.last() {
                vec![last.1].into()
            } else {
                VecDeque::with_capacity(Mm::CALL_STACK_CAP)
            },
            labels,
            flags: Flags::new(),
            program,
            ip: 0,
            halt: false,
        };

        Ok(mm)
    }

    pub fn generate_masm(&self, file_path: &str) -> std::io::Result<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        for inst in self.program.iter() {
            let inst_str = format!("{inst}\n", inst = String::from(&inst.0));
            f.write_all(&inst_str.as_bytes())?;
        }

        Ok(())
    }
}

/* TODO:
    (#8) Fix to/from binary operations with NaN-boxes in inst.rs

    1. Use lifetimes to get rid of cloning values instead of taking reference.
    2. Introduce MasmTranslator struct, that translates masm and report errors proper way.
*/

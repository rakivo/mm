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

const DEBUG: bool = true;

pub type Word = NaNBox;
pub type MResult<T> = std::result::Result<T, Trap>;

pub type Program = Vec<Inst>;
pub type Labels = std::collections::HashMap<String, usize>;

pub struct Mm {
    stack: Vec<Word>,
    labels: Labels,
    flags: Flags,
    program: Program,
    ip: usize,
    halt: bool,
}

impl std::fmt::Debug for Mm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack size: {size}\n", size = self.stack.len())?;
        write!(f, "stack:")?;
        let mut i = 0;
        while i < self.stack.len() {
            let oper = self.stack[i];
            if oper.is_u64() {
                write!(f, ", {oper}", oper = oper.as_u64())?;
            } else if oper.is_f64() {
                write!(f, ", {oper}")?;
            } else { todo!() }
            i += 1;
        }
        Ok(())
    }
}

impl std::fmt::Display for Mm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack: ")?;
        if let Some(first) = self.stack.first() {
            if first.is_u64() {
                write!(f, "{f}", f = first.as_u64())?;
            } else if first.is_f64() {
                write!(f, "{first}")?;
            } else { todo!() }

            let (mut i, n) = (1, self.stack.len());
            while i < n {
                let oper = self.stack[i];
                if oper.is_u64() {
                    write!(f, ", {oper}", oper = oper.as_u64())?;
                } else if oper.is_f64() {
                    write!(f, ", {oper}")?;
                } else { todo!() }
                i += 1;
            }
        }
        Ok(())
    }
}

impl Mm {
    const STACK_CAP: usize = 1024;

    fn process_labels_m(program: &MProgram) -> Labels {
        program
            .iter()
            .fold((Labels::new(), 0), |(mut labels, ip), inst| {
                match &inst.0 {
                    Inst::LABEL(label) => {
                        labels.insert(label.to_owned(), ip);
                    }
                    _ => {}
                }
                (labels, ip + 1)
            })
            .0
    }

    fn process_labels(program: &Program) -> Labels {
        program
            .iter()
            .fold((Labels::new(), 0), |(mut labels, ip), inst| {
                match inst {
                    Inst::LABEL(label) => {
                        labels.insert(label.to_owned(), ip);
                    }
                    _ => {}
                }
                (labels, ip + 1)
            })
            .0
    }

    pub fn new_slice(program: &[Inst]) -> Mm {
        let program = program.to_vec();
        Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
            labels: Self::process_labels(&program),
            flags: Flags::new(),
            program,
            ip: 0,
            halt: false,
        }
    }

    pub fn new(program: Program) -> Mm {
        Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
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

        let prelast = self.stack.last_mut().unwrap();
        let Some(b) = last.get_f64() else {
            return Err(Trap::DivisionOfDifferentTypes(
                prelast.get_type(),
                last.get_type(),
            ));
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

        let prelast = self.stack.last_mut().unwrap();
        let (Some(a), Some(b)) = (prelast.get_u64(), last.get_u64()) else {
            return Err(Trap::DivisionOfDifferentTypes(
                prelast.get_type(),
                last.get_type(),
            ));
        };

        use Inst::*;
        match inst {
            IADD => { *prelast = NaNBox::from_u64(a + b); }
            ISUB => { *prelast = NaNBox::from_u64(a - b); }
            IMUL => { *prelast = NaNBox::from_u64(a * b); }
            IDIV => {
                if b != 0 {
                    *prelast = NaNBox::from_u64(a / b);
                } else {
                    return Err(Trap::DivisionByZero(inst));
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
            return Err(Trap::StackUnderflow(inst));
        }

        let last = if pop {
            self.stack.pop().unwrap()
        } else {
            self.stack[stack_len - 1].to_owned()
        };

        let prelast = &mut self.stack[stack_len - 2];

        use Inst::*;
        match inst {
            SWAP => {
                let a = *prelast;
                let b = last;
                self.stack.pop();
                self.stack.push(b);
                self.stack.push(a)
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
            return Err(Trap::InvalidLabel(
                label.to_owned(),
                "Not found in label map".to_owned(),
            ));
        };

        if *ip >= program_len {
            eprintln!(
                "ERROR: operand `{ip}` is outside of program bounds, program len: {program_len}"
            );
            return Err(Trap::InvalidLabel(
                label.to_owned(),
                "Out of bounds".to_owned(),
            ));
        }

        if self.flags.is(flag) {
            self.ip = *ip;
        } else {
            self.ip += 1;
        }

        Ok(())
    }

    pub fn execute(&mut self) -> Result<(), Trap> {
        if self.ip >= self.program.len() {
            self.halt = true;
            return Ok(());
        }

        let inst = self.program[self.ip].to_owned();
        if DEBUG {
            println!("{ip}: {inst}", ip = self.ip);
        }

        use Inst::*;
        match inst {
            NOP => Ok(()),

            PUSH(oper) => {
                if self.stack.len() < Mm::STACK_CAP {
                    self.stack.push(oper);
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackOverflow(inst.to_owned()))
                }
            }

            POP => {
                if !self.stack.is_empty() {
                    self.stack.pop();
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.to_owned()))
                }
            }

            INC => {
                if let Some(last) = self.stack.last_mut() {
                    let v = last.get_value();
                    *last = NaNBox::from_i64(v + 1);
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.to_owned()))
                }
            }

            DEC => {
                if let Some(last) = self.stack.last_mut() {
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
                if let Some(ref last) = self.stack.last() {
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
                        self.stack.push(val);
                        self.ip += 1;
                        Ok(())
                    } else {
                        Err(Trap::StackOverflow(inst.to_owned()))
                    }
                } else {
                    Err(Trap::StackUnderflow(inst.to_owned()))
                }
            }

            JE(ref label) | JL(ref label) | JG(ref label) | JNGE(ref label) | JNE(ref label)
            | JNLE(ref label) | JZ(ref label) | JNZ(ref label) => {
                self.jump_if_flag(label, Flag::try_from(&inst).unwrap())
            }

            JMP(label) => {
                let Some(ip) = self.labels.get(&label) else {
                    return Err(Trap::InvalidLabel(
                        label,
                        "Not found in label map".to_owned(),
                    ));
                };

                if *ip < self.program.len() {
                    self.ip = *ip;
                    Ok(())
                } else {
                    eprintln!(
                        "ERROR: operand `{ip}` is outside of program bounds, program len: {len}",
                        len = self.program.len()
                    );
                    Err(Trap::InvalidLabel(label, "Out of bounds".to_owned()))
                }
            }

            LABEL(..) => {
                self.ip += 1;
                Ok(())
            }

            BOT => {
                if let Some(first) = self.stack.first() {
                    if self.stack.len() < Mm::STACK_CAP {
                        self.stack.push(*first);
                        Ok(())
                    } else {
                        return Err(Trap::StackOverflow(inst));
                    }
                } else {
                    return Err(Trap::StackUnderflow(inst));
                }
            }

            HALT => {
                self.halt = true;
                Ok(())
            }
        }
    }

    pub fn to_binary(&self, file_path: &str) -> std::io::Result<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        for inst in self.program.iter() {
            f.write_all(&inst.as_bytes())?;
        }

        Ok(())
    }

    pub fn from_binary(file_path: &str) -> MResult<Mm> {
        use std::fs::read;

        let buf = read(file_path)
            .map_err(|err| {
                eprintln!("Failed to read file: {file_path}: {err}");
                err
            })
            .unwrap();

        let (mut i, mut ip, mut program, mut labels) = (0, 0, Program::new(), Labels::new());
        while i < buf.len() {
            let (inst, size) = Inst::from_bytes(&buf[i..])?;
            match inst {
                Inst::LABEL(ref label) => {
                    labels.insert(label.to_owned(), ip);
                }
                _ => {}
            };
            ip += 1;
            program.push(inst);
            i += size;
        }

        if matches!(program.last(), Some(last) if *last != Inst::HALT) {
            program.push(Inst::HALT);
        }

        let mm = Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
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
            let inst_str = format!("{inst}\n", inst = String::from(inst));
            f.write_all(&inst_str.as_bytes())?;
        }

        Ok(())
    }
}

/* TODO:
    Use lifetimes to get rid of cloning values instead of taking reference
    Introduce MasmTranslator struct, that translates masm and report errors proper way.
*/

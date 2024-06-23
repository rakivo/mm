pub mod inst;
pub mod flag;
pub mod trap;

pub use inst::*;
pub use flag::*;
pub use trap::*;

pub type Word = u64;
pub type MResult<T> = std::result::Result::<T, Trap>;

pub type Program = Vec::<Inst>;
pub type Labels = std::collections::HashMap::<String, usize>;

const DEBUG: bool = false;

pub struct Mm {
    stack: Vec::<Word>,
    labels: Labels,
    flags: Flags,
    program: Program,
    ip: usize,
    halt: bool
}

impl std::fmt::Debug for Mm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack size: {size}\n", size = self.stack.len())?;
        write!(f, "stack:")?;
        let mut i = 0;
        while i < self.stack.len() {
            write!(f, "\n\t{oper}", oper = self.stack[i])?;
            i += 1;
        }
        Ok(())
    }
}

impl std::fmt::Display for Mm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack: ")?;
        if let Some(first) = self.stack.first() {
            write!(f, "{first}")?;
            let (mut i, n) = (1, self.stack.len());
            while i < n {
                write!(f, ", {oper}", oper = self.stack[i])?;
                i += 1;
            }
        }
        Ok(())
    }
}

impl Mm {
    const STACK_CAP: usize = 1024;

    fn process_labels(program: &Program) -> Labels {
        program.iter().fold((Labels::new(), 0), |(mut labels, mut ip), inst| {
            match inst {
                Inst::LABEL(label) => { labels.insert(label.to_owned(), ip); ip += 1; }
                _ => { ip += 1 }
            } (labels, ip)
        }).0
    }

    pub fn new_slice(program: &[Inst]) -> Mm {
        let program = program.to_vec();
        Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
            labels: Self::process_labels(&program),
            flags: Flags::new(),
            program,
            ip: 0,
            halt: false
        }
    }

    pub fn new(program: Program) -> Mm {
        Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
            labels: Self::process_labels(&program),
            flags: Flags::new(),
            program,
            ip: 0,
            halt: false
        }
    }

    #[inline(always)]
    pub fn halt(&self) -> &bool {
        &self.halt
    }

    fn two_opers_inst(&mut self, inst: Inst, pop: bool) -> Result<(), Trap> {
        let stack_len = self.stack.len();
        if stack_len < 2 {
            eprintln!("ERROR: Not enough operands on the stack, needed: 2, have: {stack_len}");
            eprintln!("Last executed instruction: {inst:?}");
            return Err(Trap::StackUnderflow(inst))
        }

        use Inst::*;
        let last = if pop {
            self.stack.pop().unwrap()
        } else {
            self.stack[stack_len - 1].to_owned()
        };

        let prelast = &mut self.stack[stack_len - 2];
        match inst {
            ADD => *prelast += last,
            SUB => *prelast -= last,
            MUL => *prelast *= last,
            DIV => if last != 0 {
                *prelast /= last
            } else { return Err(Trap::DivisionByZero(inst)) }
            SWAP => {
                let a = *prelast;
                let b = last;
                self.stack.pop();
                self.stack.push(b);
                self.stack.push(a);
            }
            _ => unreachable!()
        };

        self.ip += 1;
        Ok(())
    }

    fn jump_if_flag(&mut self, label: &str, flag: Flag) -> Result<(), Trap> {
        let program_len = self.program.len();
        let Some(ip) = self.labels.get(&label.to_owned()) else {
            return Err(Trap::InvalidLabel(label.to_owned(), "Not found in label map".to_owned()));
        };

        if *ip >= program_len {
            eprintln!("ERROR: operand `{ip}` is outside of program bounds, program len: {program_len}");
            return Err(Trap::InvalidLabel(label.to_owned(), "Out of bounds".to_owned()));
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
            return Ok(())
        }

        let inst = self.program[self.ip].to_owned();
        if DEBUG {
            println!("{ip}: {inst}", ip = self.ip);
        }

        use Inst::*;
        match inst {
            NOP => Ok(()),
            PUSH(oper) => if self.stack.len() < Mm::STACK_CAP {
                self.stack.push(oper);
                self.ip += 1;
                Ok(())
            } else { Err(Trap::StackOverflow(inst.to_owned())) }

            POP => if !self.stack.is_empty() {
                self.stack.pop();
                self.ip += 1;
                Ok(())
            } else { Err(Trap::StackUnderflow(inst.to_owned())) }

            INC => if let Some(last) = self.stack.last_mut() {
                *last += 1;
                self.ip += 1;
                Ok(())
            } else { Err(Trap::StackUnderflow(inst.to_owned())) }

            DEC => if let Some(last) = self.stack.last_mut() {
                *last -= 1;
                self.ip += 1;
                Ok(())
            } else { Err(Trap::StackUnderflow(inst.to_owned())) }

            ADD  => self.two_opers_inst(ADD, true),
            SUB  => self.two_opers_inst(SUB, true),
            MUL  => self.two_opers_inst(MUL, true),
            DIV  => self.two_opers_inst(DIV, true),

            CMP(oper) => if let Some(ref last) = self.stack.last() {
                self.flags.cmp(last, &oper);
                self.ip += 1;
                Ok(())
            } else { Err(Trap::StackUnderflow(inst.to_owned())) }

            SWAP => self.two_opers_inst(SWAP, true),

            DUP(oper) => if self.stack.len() > oper as usize {
                if self.stack.len() < Mm::STACK_CAP {
                    let val = self.stack[self.stack.len() - 1 - oper as usize];
                    self.stack.push(val);
                    self.ip += 1;
                    Ok(())
                } else { Err(Trap::StackOverflow(inst.to_owned())) }
            } else { Err(Trap::StackUnderflow(inst.to_owned())) }

              JE(ref oper)
            | JL(ref oper)
            | JG(ref oper)
            | JNGE(ref oper)
            | JNE(ref oper)
            | JNLE(ref oper)
            | JZ(ref oper)
            | JNZ(ref oper) => self.jump_if_flag(oper, Flag::try_from(&inst).unwrap()),

            JMP(label) => {
                let Some(ip) = self.labels.get(&label) else {
                    return Err(Trap::InvalidLabel(label, "Not found in label map".to_owned()));
                };

                if *ip < self.program.len() {
                    self.ip = *ip;
                    Ok(())
                } else {
                    eprintln!("ERROR: operand `{ip}` is outside of program bounds, program len: {len}", len = self.program.len());
                    Err(Trap::InvalidLabel(label, "Out of bounds".to_owned()))
                }
            }

            LABEL(..) => {
                self.ip += 1;
                Ok(())
            }

            BOT => if let Some(first) = self.stack.first() {
                if self.stack.len() < Mm::STACK_CAP {
                    self.stack.push(*first);
                    Ok(())
                } else {
                    return Err(Trap::StackOverflow(inst))
                }
            } else {
                return Err(Trap::StackUnderflow(inst))
            }

            HALT => {
                self.halt = true;
                Ok(())
            }
        }
    }

    pub fn to_binary(&self, file_path: &str) -> std::io::Result::<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        for inst in self.program.iter() {
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

        let (mut i, mut ip, mut program, mut labels) = (0, 0, Vec::new(), Labels::new());
        while i < buf.len() {
            let (inst, size) = Inst::from_bytes(&buf[i..])?;
            match inst {
                Inst::LABEL(ref label) => { labels.insert(label.to_owned(), ip); ip += 1 }
                _ => ip += 1
            };
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
            halt: false
        };

        Ok(mm)
    }

    pub fn from_masm(file_path: &str) -> MResult<Mm> {
        use std::{fs::read_to_string, convert::TryFrom};

        let results = read_to_string(&file_path).map_err(|err| {
            eprintln!("Failed to open file: {file_path}: {err}");
            err
        }).unwrap()
          .lines()
          .filter(|l| !l.starts_with(';') && !l.trim().is_empty())
          .map(Inst::try_from)
          .collect::<Vec::<_>>();

        let mut program = Vec::new();
        for res in results {
            program.push(res?);
        }

        if matches!(program.last(), Some(last) if *last != Inst::HALT) {
            program.push(Inst::HALT);
        }

        let labels = Self::process_labels(&program);

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

use std::collections::HashMap;

pub mod inst;
pub mod flag;
pub mod trap;

pub use inst::*;
pub use flag::*;
pub use trap::*;

pub type Word = u64;
pub type MResult<T> = std::result::Result::<T, Trap>;

pub type Program = Vec::<Inst>;

pub struct Mm {
    stack: Vec::<Word>,
    labels: HashMap::<String, usize>,
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

    pub fn new_slice(program: &[Inst]) -> Mm {
        Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
            labels: HashMap::new(),
            flags: Flags::new(),
            program: program.to_vec(),
            ip: 0,
            halt: false
        }
    }

    pub fn new(program: Program) -> Mm {
        Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
            labels: HashMap::new(),
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

    fn jump_if_flag(&mut self, label: String, flag: Flag) -> Result<(), Trap> {
        let program_len = self.program.len();
        let Some(ip) = self.labels.get(&label) else {
            return Err(Trap::InvalidLabel(label, "Not found in label map".to_owned()));
        };

        if *ip >= program_len {
            eprintln!("ERROR: operand `{ip}` is outside of program bounds, program len: {program_len}");
            return Err(Trap::InvalidLabel(label, "Out of bounds".to_owned()));
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

        use Inst::*;
        let inst = self.program[self.ip].to_owned();
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
                self.flags.cmp(&oper, last);
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

            JE(oper)   => self.jump_if_flag(oper, Flag::E),
            JL(oper)   => self.jump_if_flag(oper, Flag::L),
            JNGE(oper) => self.jump_if_flag(oper, Flag::NGE),
            JG(oper)   => self.jump_if_flag(oper, Flag::G),
            JNLE(oper) => self.jump_if_flag(oper, Flag::NLE),
            JNE(oper)  => self.jump_if_flag(oper, Flag::NE),
            JZ(oper)   => self.jump_if_flag(oper, Flag::Z),
            JNZ(oper)  => self.jump_if_flag(oper, Flag::NZ),

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

        let (mut i, mut ip, mut program, mut labels) = (0, 0, Vec::new(), HashMap::new());
        while i < buf.len() {
            let (inst, size) = Inst::from_bytes(&buf[i..])?;
            match inst {
                Inst::LABEL(label) => { labels.insert(label, ip); }
                _ => {
                    ip += 1;
                    program.push(inst)
                }
            };
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

        let (labels, results, _) = read_to_string(&file_path).map_err(|err| {
            eprintln!("Failed to open file: {file_path}: {err}");
            err
        }).unwrap()
          .lines()
          .filter(|l| !l.starts_with(';') && !l.trim().is_empty())
          .fold((HashMap::new(), Vec::new(), 0), |(mut labels, mut results, mut ip), l| {
              let inst = Inst::try_from(l);
              if let Ok(ref inst_ok) = inst {
                  match inst_ok {
                      Inst::LABEL(label) => { labels.insert(label.to_owned(), ip); }
                      _ => {
                          ip += 1;
                          results.push(inst)
                      }
                  }
              } else {
                  results.push(inst);
              }
              (labels, results, ip)
          });

        let mut program = Vec::new();
        for res in results {
            program.push(res?);
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

    pub fn generate_masm(program: &[Inst], file_path: &str) -> std::io::Result<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        for inst in program {
            let inst_str = format!("{inst}\n", inst = String::from(inst));
            f.write_all(&inst_str.as_bytes())?;
        }

        Ok(())
    }
}

/* TODO:
    Introduce MasmTranslator struct, that translates masm and report errors proper way.
*/

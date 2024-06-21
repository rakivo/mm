use std::result;

mod inst;
mod flag;
mod trap;

pub use inst::*;
pub use flag::*;
pub use trap::*;

pub type Word = u64;
pub type MResult<T> = result::Result::<T, Trap>;

pub struct Mm<'a> {
    stack: Vec::<Word>,
    flags: Flags,
    program: &'a [Inst],
    ip: usize,
    halt: bool
}

impl std::fmt::Debug for Mm<'_> {
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

impl std::fmt::Display for Mm<'_> {
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

impl<'a> Mm<'a> {
    const STACK_CAP: usize = 1024;

    pub fn new(program: &'a [Inst]) -> Mm<'a> {
        Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
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
            eprintln!("Last executed instruction: {inst:?}", inst = self.program[self.ip]);
            return Err(Trap::StackUnderflow)
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
            } else { return Err(Trap::DivisionByZero) }
            CMP => if stack_len < Mm::STACK_CAP {
                self.flags.cmp(&*prelast, &last);
            } else { return Err(Trap::StackOverflow) }
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

    fn jump_if_flag(&mut self, oper: &Word, flag: Flag) -> Result<(), Trap> {
        let program_len = self.program.len();
        if *oper as usize >= program_len {
            eprintln!("ERROR: operand {oper} is outside of program bounds, program len: {program_len}");
            return Err(Trap::InvalidOperand(Some(oper.to_string())));
        }

        if self.flags.is(flag) {
            self.ip = *oper as usize;
        } else {
            self.ip += 1;
        }
        self.flags.reset();
        Ok(())
    }

    pub fn execute(&mut self) -> Result<(), Trap> {
        if self.ip >= self.program.len() {
            self.halt = true;
            return Ok(())
        }

        use Inst::*;
        let inst = &self.program[self.ip];
        match inst {
            NOP => Ok(()),
            PUSH(oper) => if self.stack.len() < Mm::STACK_CAP {
                self.stack.push(*oper);
                self.ip += 1;
                Ok(())
            } else { Err(Trap::StackOverflow) }
            POP => if self.stack.len() >= 1 {
                self.stack.pop();
                self.ip += 1;
                Ok(())
            } else { Err(Trap::StackUnderflow) }

            ADD  => self.two_opers_inst(ADD, true),
            SUB  => self.two_opers_inst(SUB, true),
            MUL  => self.two_opers_inst(MUL, true),
            DIV  => self.two_opers_inst(DIV, true),
            CMP  => self.two_opers_inst(CMP, false),
            SWAP => self.two_opers_inst(SWAP, true),

            DUP(oper) => if self.stack.len() > *oper as usize {
                if self.stack.len() < Mm::STACK_CAP {
                    let val = self.stack[self.stack.len() - 1 - *oper as usize];
                    self.stack.push(val);
                    self.ip += 1;
                    Ok(())
                } else { Err(Trap::StackOverflow) }
            } else { Err(Trap::StackUnderflow) }

            JE(oper)   => self.jump_if_flag(oper, Flag::E),
            JL(oper)   => self.jump_if_flag(oper, Flag::L),
            JNGE(oper) => self.jump_if_flag(oper, Flag::NGE),
            JG(oper)   => self.jump_if_flag(oper, Flag::G),
            JNLE(oper) => self.jump_if_flag(oper, Flag::NLE),
            JZ(oper)   => self.jump_if_flag(oper, Flag::Z),
            JNZ(oper)  => self.jump_if_flag(oper, Flag::NZ),

            JMP(oper) => if (*oper as usize) < self.program.len() {
                self.ip = *oper as usize;
                Ok(())
            } else {
                eprintln!("ERROR: operand {oper} is outside of program bounds, program len: {len}", len = self.program.len());
                Err(Trap::IllegalInstructionAccess)
            }

            HALT => {
                self.halt = true;
                Ok(())
            }
        }
    }

    pub fn save_program_to_file(program: &[Inst], file_path: &str) -> std::io::Result::<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        for inst in program {
            f.write_all(&inst.as_bytes())?;
        }

        Ok(())
    }

    pub fn load_program_from_file(file_path: &str) -> std::io::Result<Vec::<Inst>> {
        use std::{fs::read, io::*};

        let buf = read(file_path)?;
        let (mut i, mut program) = (0, Vec::new());
        while i < buf.len() {
            match Inst::from_bytes(&buf[i..]) {
                Ok((inst, size)) => {
                    program.push(inst);
                    i += size;
                }
                Err(e) => return Err(Error::new(ErrorKind::InvalidData, format!("{e:?}")))
            }
        }

        Ok(program)
    }

    pub fn translate_masm(file_path: &str) -> MResult<Vec::<Inst>> {
        use std::{fs::read_to_string, convert::TryFrom};

        let results = read_to_string(&file_path).map_err(|err| {
            eprintln!("Failed to open file: {file_path}: {err}");
            err
        }).unwrap()
          .lines()
          .filter(|l| !l.starts_with(";") && !l.trim().is_empty())
          .map(Inst::try_from)
          .collect::<Vec::<_>>();

        let mut ret = Vec::new();
        for res in results {
            ret.push(res?);
        }

        Ok(ret)
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

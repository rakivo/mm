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

const ENTRY_POINT_FUNCTION: &str = "_start";

pub type Word = NaNBox;
pub type MResult<T> = std::result::Result<T, Trap>;

pub type Program = Vec<Inst>;
pub type Labels = std::collections::HashMap<String, usize>;
pub type Funcs = std::collections::HashMap<String, usize>;

pub struct Mm {
    stack: Vec::<Word>,
    call_stack: Vec::<usize>,

    funcs: Funcs,
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
        if let Some(first) = self.stack.first() {
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
    const STACK_CAP: usize = 1024;

    fn process_funcs_m(program: &MProgram) -> Funcs {
        program.iter().fold((Funcs::new(), 0), |(mut funcs, ip), (inst, _)| {
            Self::process_func(inst, &mut funcs, ip);
            (funcs, ip + 1)
        }).0
    }

    fn process_func(inst: &Inst, funcs: &mut Funcs, ip: usize) {
        match inst {
            Inst::FUNC(func) => { funcs.insert(func.to_owned(), ip); }
            _ => {}
        }
    }

    fn process_funcs(program: &Program) -> Funcs {
        program.iter().fold((Funcs::new(), 0), |(mut funcs, ip), inst| {
            Self::process_func(inst, &mut funcs, ip);
            (funcs, ip + 1)
        }).0
    }

    fn process_labels_m(program: &MProgram) -> Labels {
        program.iter().fold((Labels::new(), 0), |(mut labels, ip), (inst, _)| {
            Self::process_label(inst, &mut labels, ip);
            (labels, ip + 1)
        }).0
    }

    fn process_label(inst: &Inst, labels: &mut Labels, ip: usize) {
        match inst {
            Inst::LABEL(label) => { labels.insert(label.to_owned(), ip); }
            _ => {}
        }
    }

    fn process_labels(program: &Program) -> Labels {
        program.iter().fold((Labels::new(), 0), |(mut labels, ip), inst| {
            Self::process_label(inst, &mut labels, ip);
            (labels, ip + 1)
        }).0
    }

    pub fn new_slice(program: &[Inst]) -> Mm {
        let program = program.to_vec();
        Mm {
            stack: Vec::with_capacity(Mm::STACK_CAP),
            call_stack: Vec::with_capacity(Mm::STACK_CAP),
            funcs: Self::process_funcs(&program),
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
            call_stack: Vec::with_capacity(Mm::STACK_CAP),
            funcs: Self::process_funcs(&program),
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

        let prelast = self.stack.last_mut().unwrap();
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
            return Err(Trap::InvalidLabel(label.to_owned(), "Not found in label map".to_owned()))
        };

        if *ip >= program_len {
            eprintln!(
                "ERROR: operand `{ip}` is outside of program bounds, program len: {program_len}"
            );
            return Err(Trap::InvalidLabel(label.to_owned(), "Out of bounds".to_owned()))
        }

        if self.flags.is(flag) {
            self.ip = *ip;
        } else {
            self.ip += 1;
        }

        Ok(())
    }

    fn execute_instruction(&mut self) -> Result<(), Trap> {
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
                    eprintln!(
                        "ERROR: operand `{ip}` is outside of program bounds, program len: {len}",
                        len = self.program.len()
                    );
                    Err(Trap::InvalidLabel(label, "Out of bounds".to_owned()))
                }
            }


            BOT => {
                if let Some(first) = self.stack.first() {
                    if self.stack.len() < Mm::STACK_CAP {
                        self.stack.push(*first);
                        self.ip += 1;
                        Ok(())
                    } else {
                        return Err(Trap::StackOverflow(inst))
                    }
                } else {
                    return Err(Trap::StackUnderflow(inst))
                }
            }

            DMP(stream) => if let Some(last) = self.stack.last() {
                match stream {
                    1 => print_oper_s(std::io::stdout(), last).unwrap(),
                    2 => print_oper_s(std::io::stderr(), last).unwrap(),
                    _ => return Err(Trap::InvalidOperand(InstString(inst.to_string(), Some(stream.to_string()))))
                }
                self.ip += 1;
                println!();
                Ok(())
            } else {
                return Err(Trap::StackUnderflow(inst))
            }

            CALL(addr) => {
                if self.program.len() > self.ip {
                    self.call_stack.push(self.ip + 1);
                }

                if let Some(ip) = self.funcs.get(&addr) {
                    self.ip = *ip;
                    Ok(())
                } else {
                    return Err(Trap::InvalidFunction(addr.to_owned(), "Not found in function map".to_owned()))
                }
            }

            RET => {
                if let Some(ip) = self.call_stack.pop() {
                    self.ip = ip;
                    Ok(())
                } else {
                    panic!("Return address not found in stack")
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

    pub fn execute_program(&mut self, debug: bool, limit: Option::<usize>) -> Result<(), Trap> {
        let mut count = 0;
        let limit = limit.unwrap_or(usize::MAX);
        while !self.halt() && count < limit {
            self.execute_instruction()?;

            if debug {
                println!("{self}");
            }

            count += 1;
        }

        Ok(())
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

        let buf = read(file_path).map_err(|err| {
            eprintln!("Failed to read file: {file_path}: {err}");
            err
        }).unwrap();

        let (mut i, mut ip, mut program, mut labels, mut funcs) = (0, 0, Program::new(), Labels::new(), Funcs::new());
        while i < buf.len() {
            let (inst, size) = Inst::from_bytes(&buf[i..])?;
            match inst {
                Inst::LABEL(ref label) => { labels.insert(label.to_owned(), ip); }
                Inst::FUNC(ref func) => { funcs.insert(func.to_owned(), ip); }
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
            call_stack: Vec::with_capacity(Mm::STACK_CAP),
            funcs,
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
    (#7) Change std::vec::Vec type to std::collections::VecDeque in the Mm struct to speed up runtime

    (#6) Add debug messages like `started parsing at: hh:mm::ss`, `started execution at ...`.

    1. Use lifetimes to get rid of cloning values instead of taking reference.
    2. Introduce MasmTranslator struct, that translates masm and report errors proper way.
*/

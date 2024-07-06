use std::{borrow::Cow, time::Instant, collections::VecDeque};

pub mod nan;
pub mod flag;
pub mod inst;
pub mod trap;
pub mod lexer;
pub mod parser;
pub mod libloading;

pub use nan::*;
pub use flag::*;
pub use inst::*;
pub use trap::*;
pub use lexer::*;
pub use parser::*;
pub use libloading::*;

const DEBUG: bool = false;

pub type MResult<T> = std::result::Result<T, Trap>;

pub type Program = Vec<(Loc, Inst)>;

pub type Labels = std::collections::HashMap<String, usize>;
pub type Externs = std::collections::HashMap::<String, (*const (), usize)>;

pub struct Mm {
    pub file_path: String,

    stack: VecDeque::<NaNBox>,
    call_stack: VecDeque::<usize>,

    externs: Externs,

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
            write!(f, ", {oper}")?;
            i += 1;
        }
        Ok(())
    }
}

impl std::fmt::Display for Mm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack: ")?;
        if let Some(first) = self.stack.front() {
            write!(f, "{first}")?;
            let (mut i, n) = (1, self.stack.len());
            while i < n {
                let oper = self.stack[i];
                write!(f, ", {oper}")?;
                i += 1;
            }
        }
        Ok(())
    }
}

impl Mm {
    const STACK_CAP: usize = 8 * 1024;
    const CALL_STACK_CAP: usize = 8 * 128;

    fn process_labels(program: &Program, f: &str) -> Labels {
        program.iter().fold((Labels::new(), 0), |(mut labels, ip), (loc, inst)| {
            match inst.typ {
                InstType::LABEL => if labels.contains_key(inst.val.as_string()) {
                    panic!("{f}:{r}:{c}: <-- Here, conflicting definitions of: {n:?}", n = inst.val.as_string(), r = loc.0 + 1, c = loc.1)
                } else {
                    labels.insert(inst.val.as_string().to_owned(), ip);
                }
                _ => {}
            }
            (labels, ip + 1)
        }).0
    }

    fn process_externs(program: &Program, libs: &Vec::<*mut void>) -> Externs {
        program.iter().filter(|x| x.1.typ == InstType::EXTERN).fold(Externs::new(), |mut exs, (_, inst)| {
            let (sym, args_count) = inst.val.as_string_u64();
            let ex = libs.iter().cloned().find_map(|l| load_sym(l, &sym).ok()).expect(&format!("No such symbol: {sym}"));
            exs.insert(sym.to_owned(), (ex, args_count as usize));
            exs
        })
    }

    #[inline(always)]
    pub fn halt(&self) -> &bool {
        &self.halt
    }

    fn two_opers_finst(&mut self, inst: InstType, last: NaNBox) -> MResult<()> {
        assert!(self.stack.len() > 0);

        let prelast = self.stack.back_mut().unwrap();

        use InstType::*;
        match inst {
            FADD => prelast.0 += last.0,
            FSUB => prelast.0 -= last.0,
            FMUL => prelast.0 *= last.0,
            FDIV => prelast.0 /= last.0,
            _ => unreachable!()
        }

        Ok(())
    }

    fn two_opers_iinst(&mut self, inst: InstType, last: NaNBox) -> MResult<()> {
        assert!(self.stack.len() > 0);

        let prelast = self.stack.back_mut().unwrap();
        let (Some(a), Some(b)) = (prelast.get_i64(), last.get_i64()) else {
            return Err(Trap::OperationWithDifferentTypes(prelast.get_type(), last.get_type()))
        };

        use InstType::*;
        match inst {
            IADD => { *prelast = NaNBox::from_i64(a + b); }
            ISUB => { *prelast = NaNBox::from_i64(a - b); }
            IMUL => { *prelast = NaNBox::from_i64(a * b); }
            IDIV => {
                if b != 0 {
                    *prelast = NaNBox::from_i64(a / b);
                } else {
                    return Err(Trap::DivisionByZero(inst))
                }
            }
            _ => unreachable!()
        }
        *prelast = NaNBox(NaNBox::set_type(prelast.0, Type::I64));

        Ok(())
    }

    fn two_opers_inst(&mut self, inst: InstType, pop: bool, typeflag: u8) -> MResult<()> {
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

        use InstType::*;
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

    fn execute_instruction(&mut self, inst: Inst) -> Result::<(), Trap> {
        if DEBUG {
            println!("{ip}: {inst}", ip = self.ip);
        }

        use InstType::*;
        match inst.typ {
            NOP => Ok(()),

            PUSH => {
                let oper = inst.val.as_nan();
                if self.stack.len() < Mm::STACK_CAP {
                    self.stack.push_back(*oper);
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackOverflow(inst.typ))
                }
            }

            POP => {
                if !self.stack.is_empty() {
                    self.stack.pop_back();
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.typ))
                }
            }

            INC => {
                if let Some(last) = self.stack.back_mut() {
                    let v = last.get_value();
                    *last = NaNBox::from_i64(v + 1);
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.typ))
                }
            }

            DEC => {
                if let Some(last) = self.stack.back_mut() {
                    let v = last.get_value();
                    *last = NaNBox::from_i64(v - 1);
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.typ))
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

            CMP => {
                let oper = inst.val.as_nan();
                if let Some(ref last) = self.stack.back() {
                    self.flags.cmp(&last.as_u64(), &oper.as_u64());
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::StackUnderflow(inst.typ))
                }
            }

            DUP => {
                let oper = inst.val.as_u64();
                if self.stack.len() > *oper as usize {
                    if self.stack.len() < Mm::STACK_CAP {
                        let val = self.stack[self.stack.len() - 1 - *oper as usize];
                        self.stack.push_back(val);
                        self.ip += 1;
                        Ok(())
                    } else {
                        Err(Trap::StackOverflow(inst.typ))
                    }
                } else {
                    Err(Trap::StackUnderflow(inst.typ))
                }
            }

              JE
            | JL
            | JG
            | JNGE
            | JNE
            | JNLE
            | JZ
            | JNZ => self.jump_if_flag(inst.val.as_string(), Flag::try_from(&inst.typ).unwrap()),

            JMP => {
                let label = inst.val.as_string();
                let Some(ip) = self.labels.get(label) else {
                    return Err(Trap::InvalidLabel(label.to_owned(), "Not found in label map".to_owned()))
                };

                if *ip < self.program.len() {
                    self.ip = *ip;
                    Ok(())
                } else {
                    eprintln!("ERROR: operand `{ip}` is outside of program bounds, program len: {len}",
                              len = self.program.len());
                    Err(Trap::InvalidLabel(label.to_owned(), "Out of bounds".to_owned()))
                }
            }

            BOT => {
                if let Some(first) = self.stack.front() {
                    if self.stack.len() < Mm::STACK_CAP {
                        self.stack.push_back(*first);
                        self.ip += 1;
                        Ok(())
                    } else {
                        Err(Trap::StackOverflow(inst.typ))
                    }
                } else {
                    Err(Trap::StackUnderflow(inst.typ))
                }
            }

            DMP => if let Some(last) = self.stack.back() {
                use std::io::Write;

                let stream = inst.val.as_u8();
                match stream {
                    1 => write!(std::io::stdout(), "{last}").unwrap(),
                    2 => write!(std::io::stderr(), "{last}").unwrap(),
                    _ => return Err(Trap::InvalidOperand(inst.to_string()))
                }
                self.ip += 1;
                println!();
                Ok(())
            } else {
                Err(Trap::StackUnderflow(inst.typ))
            }

            CALL => {
                let addr = inst.val.as_string();
                if self.program.len() > self.ip {
                    if self.call_stack.len() < Self::CALL_STACK_CAP {
                        self.call_stack.push_back(self.ip + 1);
                    } else {
                        return Err(Trap::CallStackOverflow(inst))
                    }
                }

                if let Some(ip) = self.labels.get(addr) {
                    self.ip = *ip;
                    Ok(())
                } else if let Some((..)) = self.externs.get(addr) {
                    todo!("Calling external functions feature is unimplemented");
                    // if self.stack.len() < *args_count { return Err(Trap::StackUnderflow(inst.typ)) }
                    // self.ip += 1;
                    // Ok(())
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
        if self.program.is_empty() {
            return Ok(())
        }

        let time = Instant::now();

        let mut count = 0;
        let file_path: Cow<str> = self.file_path.to_owned().into();
        let limit = limit.unwrap_or(usize::MAX);
        while !self.halt() && count < limit {
            let (loc, inst) = self.program[self.ip].to_owned();
            self.execute_instruction(inst).map_err(|trap| {
                MTrap::from((file_path.to_owned(), loc, trap))
            })?;

            if debug {
                println!("{self}");
            }

            count += 1;
        }

        let elapsed = time.elapsed().as_micros();
        println!("Execution of the program took: {elapsed}ms");

        Ok(())
    }

    pub fn to_binary(&self, file_path: &str) -> std::io::Result<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        let time = Instant::now();

        for (_, inst) in self.program.iter() {
            f.write_all(&inst.as_bytes())?;
        }

        let elapsed = time.elapsed().as_micros();
        println!("Compiling to binary took: {elapsed}ms");

        Ok(())
    }

    pub fn from_binary(file_path: &str) -> MResult::<Mm> {
        let buf = std::fs::read(file_path).map_err(|err| {
            eprintln!("Failed to read file: {file_path}: {err}");
            err
        }).unwrap();

        let time = Instant::now();

        let (mut i, mut ip, mut program, mut labels) = (0, 0, Program::new(), Labels::new());
        while i < buf.len() {
            let (inst, size) = Inst::from_bytes(&buf[i..])?;
            match inst.typ {
                InstType::LABEL => { labels.insert(inst.val.as_string().to_owned(), ip); }
                _ => {}
            };
            program.push(((69, 69), inst));
            i += size;
            ip += 1;
        }

        if matches!(program.last(), Some(last) if last.1.typ != InstType::HALT) {
            let inst = Inst { typ: InstType::HALT, val: InstValue::None };
            program.push(((69, 69), inst));
        }

        let elapsed = time.elapsed().as_micros();
        println!("Compiling from binary took: {elapsed}ms");

        let mm = Mm {
            file_path: file_path.to_owned(),
            stack: VecDeque::with_capacity(Mm::STACK_CAP),
            call_stack: if let Some(last) = program.last() {
                vec![last.0.0].into()
            } else {
                VecDeque::with_capacity(Mm::CALL_STACK_CAP)
            },
            externs: Externs::new(),
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
        let time = Instant::now();

        for inst in self.program.iter() {
            let inst_str = format!("{inst}\n", inst = String::from(&inst.1));
            f.write_all(&inst_str.as_bytes())?;
        }

        let elapsed = time.elapsed().as_micros();
        println!("Generation took: {elapsed} microseconds");

        Ok(())
    }
}

/* TODO:
    (#12) Implement proper errors and do not just `panic!`, even more embed lexer into the VM, to get even better error messages.
    (#13) Allow use of macros inside of macros.
    (#14) Introduce notes to errors.

    1. Use lifetimes to get rid of cloning values instead of taking reference.
    2. Introduce MasmTranslator struct, that translates masm and report errors proper way.
*/

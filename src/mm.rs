use std::{
    borrow::Cow,
    time::Instant,
    collections::{HashMap, VecDeque},
};

pub mod nan;
pub mod flag;
pub mod inst;
pub mod trap;
pub mod lexer;
pub mod parser;

pub use nan::*;
pub use flag::*;
pub use inst::*;
pub use trap::*;
pub use lexer::*;
pub use parser::*;

#[cfg(feature = "dbg")]
const DEBUG: bool = true;

#[cfg(not(feature = "dbg"))]
const DEBUG: bool = false;

pub type MResult<'a, T> = std::result::Result<T, Trap<'a>>;

pub type Program = Vec::<(Loc, Inst)>;

pub type Labels = HashMap::<String, usize>;

pub type NativeFn = for<'a, 'b> fn(&'a mut Mm<'b>);
pub type Natives = HashMap::<&'static str, NativeFn>;

pub type Lib = (*const (), usize);
pub type Libs = Vec::<Lib>;

pub type Externs = HashMap::<String, Lib>;

const ENTRY_POINT: &str = "_start";

pub const MEMORY_CAP: usize = 8 * 128;

pub struct Mm<'a> {
    pub file_path: &'a str,

    stack: VecDeque::<NaNBox>,
    call_stack: VecDeque::<usize>,

    natives: Natives,
    externs: Externs,

    memory: [u8; MEMORY_CAP],

    labels: Labels,
    flags: Flags,
    ip: usize,
    halt: bool,
}

impl std::fmt::Debug for Mm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack size: {size}\n", size = self.stack.len())?;
        write!(f, ", stack: {:?}", self.stack)?;
        write!(f, ", call stack: {:?}", self.call_stack)?;
        write!(f, ", memory: {:?}", &self.memory[0..50])?;
        Ok(())
    }
}

impl std::fmt::Display for Mm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack: {:?}", self.stack)?;
        write!(f, ", call stack: {:?}", self.call_stack)?;
        write!(f, ", memory: {:?}", &self.memory[0..50])?;
        Ok(())
    }
}

#[macro_export]
macro_rules! natives {
    ($($n: tt), *) => {{
        let mut natives = std::collections::HashMap::<&'static str, for<'a, 'b> fn(&'a mut mm::Mm<'b>)>::new();
        $(natives.insert(stringify!($n), $n);)*
            natives
    }};
}

macro_rules! convert {
    ($s: expr, $inst: expr, $f: tt, $f1: tt, $expected: tt) => {
        if let Some(l) = $s.stack.pop_back() {
            Self::typecheck(l, Type::$expected)?;
            let v = l.$f();
            $s.stack.push_back(NaNBox::$f1(v as _));
            $s.ip += 1;
            Ok(())
        } else {
            Err(Trap::StackUnderflow($inst.typ.to_owned()))
        }
    };
    (i.$s: expr, $inst: expr, $f1: tt, $m: expr, $f2: tt, $expected: tt) => {
        if let Some(l) = $s.stack.pop_back() {
            Self::typecheck(l, Type::$expected)?;
            let v = l.$f1();
            let got = l.get_type().expect("Failed to get type");
            if v < $m {
                return Err(Trap::FailedConversion(l, got, Type::$expected))
            }
            $s.stack.push_back(NaNBox::$f2(v as _));
            $s.ip += 1;
            Ok(())
        } else {
            Err(Trap::StackUnderflow($inst.typ.to_owned()))
        }
    };
}

macro_rules! readop {
    ($s: expr, $inst: expr, $ty: ty) => {
        if let Some(last) = $s.stack.pop_back() {
            let addr = last.as_u64() as usize;
            if addr >= MEMORY_CAP {
                return Err(Trap::IllegalMemoryAccess($inst.typ.to_owned(), addr))
            }
            let tmp: $ty = $s.memory[addr] as $ty;
            $s.stack.push_back(NaNBox::from_u64(tmp as _));
            $s.ip += 1;
            Ok(())
        } else {
            Err(Trap::StackUnderflow($inst.typ.to_owned()))
        }
    };
}

macro_rules! writeop {
    ($s: expr, $inst: expr, $ty: ty) => {{
        if $s.stack.len() < 2 {
            return Err(Trap::StackUnderflow($inst.typ.to_owned()))
        }
        let last: $ty = $s.stack.pop_back().unwrap().as_u64() as $ty;
        let prelast = $s.stack.pop_back().unwrap();
        let addr = prelast.as_u64() as usize;
        if addr >= MEMORY_CAP {
            return Err(Trap::IllegalMemoryAccess($inst.typ.to_owned(), addr))
        }
        $s.memory[addr] = last as u8;
        $s.ip += 1;
        Ok(())
    }};
}

impl<'a> Mm<'a> {
    const CALL_STACK_CAP: usize = 8 * 128;

    fn process_labels(program: &Program, f: &str) -> Labels {
        program.iter().fold((Labels::with_capacity(10), 0), |(mut labels, ip), (loc, inst)| {
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

    // fn process_externs(program: &Program, libs: &Vec::<*mut void>) -> Externs {
    //     program.iter().filter(|x| x.1.typ == InstType::EXTERN).fold(Externs::with_capacity(15), |mut exs, (_, inst)| {
    //         let (sym, args_count) = inst.val.as_string_u64();
    //         let ex = libs.iter().cloned().find_map(|l| load_sym(l, &sym).ok()).expect(&format!("No such symbol: {sym}"));
    //         exs.insert(sym.to_owned(), (ex, args_count as usize));
    //         exs
    //     })
    // }

    fn check_natives(program: &'a Program, natives: &'a Natives) -> MResult::<'a, ()> {
        if let Some(unative) = program.iter().filter(|x| x.1.typ == InstType::NATIVE).find(|(_, inst)| {
            !natives.contains_key(inst.val.as_string().as_str())
        }) {
            Err(Trap::UndeclaredNative(unative.1.val.as_string()))
        } else {
            Ok(())
        }
    }

    #[inline]
    fn typecheck(val: NaNBox, expected: Type) -> MResult::<'a, ()> {
        let got = val.get_type().unwrap();
        match (&got, &expected) {
            (Type::I64, Type::I64) |
            (Type::U64, Type::I64) |
            (Type::I64, Type::U64) |
            (Type::U64, Type::U64) => Ok(()),
            _ => if got != expected {
                Err(Trap::InvalidType(val, got, expected))
            } else {
                Ok(())
            }
        }
    }

    #[inline(always)]
    pub fn halt(&self) -> &bool {
        &self.halt
    }

    #[inline(always)]
    pub fn stack(&self) -> &VecDeque::<NaNBox> {
        &self.stack
    }

    #[inline(always)]
    pub fn stack_mut(&mut self) -> &mut VecDeque::<NaNBox> {
        &mut self.stack
    }

    fn two_opers_finst(&mut self, inst: InstType, last: NaNBox) -> MResult<'static, ()> {
        let prelast = self.stack.back_mut().unwrap();

        match inst {
            InstType::FADD => prelast.0 += last.0,
            InstType::FSUB => prelast.0 -= last.0,
            InstType::FMUL => prelast.0 *= last.0,
            InstType::FDIV => prelast.0 /= last.0,
            _ => unreachable!()
        }

        self.ip += 1;
        Ok(())
    }

    fn two_opers_iinst(&mut self, inst: InstType, last: NaNBox) -> MResult<'static, ()> {
        let prelast = self.stack.back_mut().unwrap();
        let (Some(a), Some(b)) = (prelast.get_i64(), last.get_i64()) else {
            return Err(Trap::OperationWithDifferentTypes(prelast.get_type().unwrap(), last.get_type().unwrap()))
        };

        match inst {
            InstType::IADD => { *prelast = NaNBox::from_i64(a + b) }
            InstType::ISUB => { *prelast = NaNBox::from_i64(a - b) }
            InstType::IMUL => { *prelast = NaNBox::from_i64(a * b) }
            InstType::IDIV => if b != 0 {
                *prelast = NaNBox::from_i64(a / b);
            } else {
                return Err(Trap::DivisionByZero(inst))
            }
            _ => unreachable!()
        }

        *prelast = NaNBox(NaNBox::set_type(prelast.0, Type::I64));
        self.ip += 1;

        Ok(())
    }

    fn two_opers_inst(&mut self, inst: InstType, pop: bool, typeflag: u8) -> MResult<'static, ()> {
        let stack_len = self.stack.len();
        if stack_len < 2 {
            eprintln!("ERROR: Not enough operands on the stack, needed: 2, have: {stack_len}");
            eprintln!("Last executed instruction: {inst}");
            return Err(Trap::StackUnderflow(inst))
        }

        let last = if pop {
            self.stack.pop_back().unwrap()
        } else {
            *self.stack.back().unwrap()
        };

        match inst {
            _ => match typeflag {
                1 => self.two_opers_iinst(inst, last)?,
                2 => self.two_opers_finst(inst, last)?,
                _ => unreachable!(),
            },
        }

        Ok(())
    }

    fn jump_if_flag(&mut self, label: &'a str, flag: Flag, program: &'a Program) -> Result<(), Trap<'a>> {
        let program_len = program.len();
        let Some(ip) = self.labels.get(label) else {
            return Err(Trap::InvalidLabel(label, "Not found in label map"))
        };

        if *ip >= program_len {
            eprintln!("ERROR: operand `{ip}` is outside of program bounds, program len: {program_len}");
            return Err(Trap::InvalidLabel(label, "Out of bounds"))
        }

        if self.flags.is(flag) {
            self.ip = *ip;
        } else {
            self.ip += 1;
        }

        Ok(())
    }

    fn execute_instruction(&mut self, program: &'a Program) -> Result::<(), Trap<'a>> {
        let inst = &program[self.ip].1;

        if DEBUG {
            println!("{ip}: {inst}, {self}", ip = self.ip);
        }

        use InstType::*;
        match inst.typ {
            PUSH => {
                self.stack.push_back(*inst.val.as_nan());
                self.ip += 1;
                Ok(())
            }

            POP => self.stack.pop_back().map_or(Err(Trap::StackUnderflow(inst.typ.to_owned())), |_| {
                self.ip += 1;
                Ok(())
            }),

            INC => self.stack.back_mut().map_or(Err(Trap::StackUnderflow(inst.typ.to_owned())), |last| {
                *last = NaNBox::from_i64(last.get_value() + 1);
                self.ip += 1;
                Ok(())
            }),

            DEC => self.stack.back_mut().map_or(Err(Trap::StackUnderflow(inst.typ.to_owned())), |last| {
                *last = NaNBox::from_i64(last.get_value() - 1);
                self.ip += 1;
                Ok(())
            }),

            SWAP => {
                let pos = *inst.val.as_u64() as usize;
                if self.stack.len() <= pos {
                    return Err(Trap::StackUnderflow(inst.typ.to_owned()))
                }

                let idx = self.stack.len() - pos - 1;
                self.stack.swap(idx, self.stack.len() - 1);
                self.ip += 1;
                Ok(())
            }

            IADD => self.two_opers_inst(IADD, true, 1),
            ISUB => self.two_opers_inst(ISUB, true, 1),
            IMUL => self.two_opers_inst(IMUL, true, 1),
            IDIV => self.two_opers_inst(IDIV, true, 1),

            FADD => self.two_opers_inst(FADD, true, 2),
            FSUB => self.two_opers_inst(FSUB, true, 2),
            FMUL => self.two_opers_inst(FMUL, true, 2),
            FDIV => self.two_opers_inst(FDIV, true, 2),

            CMP => if self.stack.len() > 1 {
                let prelast = self.stack[self.stack.len() - 2];
                let last = self.stack.pop_back().unwrap();
                self.flags.cmp(prelast.as_u64(), last.as_u64());
                self.ip += 1;
                Ok(())
            } else {
                Err(Trap::StackUnderflow(inst.typ.to_owned()))
            }

            DUP => {
                let pos = *inst.val.as_u64() as usize;
                if self.stack.len() <= pos {
                    return Err(Trap::StackUnderflow(inst.typ.to_owned()))
                }
                let idx = self.stack.len() - pos - 1;
                let val = self.stack[idx];
                self.stack.push_back(val);
                self.ip += 1;
                Ok(())
            }

            JE
                | JL
                | JG
                | JLE
                | JNE
                | JGE
                | JZ
                | JNZ => self.jump_if_flag(&inst.val.as_string(), Flag::try_from(&inst.typ).unwrap(), program),

            JMP => {
                let label = inst.val.as_string();
                let Some(ip) = self.labels.get(label) else {
                    return Err(Trap::InvalidLabel(label, "Not found in label map"))
                };

                if *ip < program.len() {
                    self.ip = *ip;
                    Ok(())
                } else {
                    eprintln!("ERROR: operand `{ip}` is outside of program bounds, program len: {len}", len = program.len());
                    Err(Trap::InvalidLabel(label, "Out of bounds"))
                }
            }

            BOT => {
                let Some(f) = self.stack.front() else { return Err(Trap::StackUnderflow(inst.typ.to_owned())) };
                self.stack.push_back(*f);
                self.ip += 1;
                Ok(())
            }

            DMP => if let Some(last) = self.stack.back() {
                use std::io::Write;

                let stream = inst.val.as_u8();
                let mut out = match stream {
                    1 => std::io::stdout().lock(),
                    2 => std::io::stdout().lock(),
                    _ => return Err(Trap::InvalidOperand(&inst.val.as_string()))
                };

                write!(out, "{last}\n").unwrap();
                out.flush().unwrap();

                self.ip += 1;
                Ok(())
            } else {
                Err(Trap::StackUnderflow(inst.typ.to_owned()))
            }

            CALL => {
                let addr = inst.val.as_string();
                if program.len() > self.ip {
                    self.call_stack.push_back(self.ip + 1);
                }

                if let Some(ip) = self.labels.get(addr) {
                    self.ip = *ip;
                    Ok(())
                } else if let Some((..)) = self.externs.get(addr.as_str()) {
                    todo!("Calling external functions feature is unimplemented");
                    // if self.stack.len() < *args_count { return Err(Trap::StackUnderflow(inst.typ)) }
                    // self.ip += 1;
                    // Ok(())
                } else if let Some(native) = self.natives.get(addr.as_str()) {
                    native(self);
                    self.ip += 1;
                    Ok(())
                } else {
                    Err(Trap::InvalidFunction(addr, "Not found in labels | natives | externs map"))
                }
            }

            RET => self.call_stack.pop_back().map_or(Err(Trap::CallStackUnderflow(inst)), |ip| {
                self.ip = ip;
                Ok(())
            }),

            F2I => convert!(self, inst, as_f64, from_i64, F64),
            F2U => convert!(i.self, inst, as_f64, 0.0, from_u64, F64),
            I2F => convert!(i.self, inst, as_i64, 0, from_f64, I64),
            I2U => convert!(i.self, inst, as_i64, 0, from_u64, I64),
            U2I => convert!(self, inst, as_u64, from_i64, U64),
            U2F => convert!(self, inst, as_u64, from_f64, U64),

            READ8  => readop!(self, inst, u8),
            READ16 => readop!(self, inst, u16),
            READ32 => readop!(self, inst, u32),
            READ64 => readop!(self, inst, u64),

            WRITE8  => writeop!(self, inst, u8),
            WRITE16 => writeop!(self, inst, u16),
            WRITE32 => writeop!(self, inst, u32),
            WRITE64 => writeop!(self, inst, u64),

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

    pub fn execute_program(&mut self, _debug: bool, limit: Option::<usize>, program: &'a Program) -> MMResult<'a, ()> {
        if program.is_empty() {
            return Ok(())
        }

        let time = Instant::now();

        let file_path: Cow::<str> = self.file_path.to_owned().into();
        let mut limit = limit.unwrap_or(usize::MAX);

        #[cfg(feature = "dbg")]
        {
            let mut times = HashMap::<&InstType, u128>::new();
        }

        while !self.halt() && limit > 0 {
            let loc = program[self.ip].0;

            #[cfg(feature = "dbg")]
            {
                let typ = &program[self.ip].1.typ;
                let time = Instant::now();
            }

            self.execute_instruction(program).map_err(|trap| {
                MTrap::from((file_path.to_owned(), loc, trap))
            })?;

            #[cfg(feature = "dbg")]
            {
                let elapsed = inst_time.elapsed().as_micros();
                times.entry(inst_type).and_modify(|e| {
                    if *e < elapsed_time {
                        *e = elapsed_time;
                    }
                }).or_insert(elapsed_time);
            }

            limit -= 1;
        }

        let elapsed = time.elapsed().as_micros();
        println!("Execution of the program took: {elapsed}ms");

        #[cfg(feature = "dbg")]
        {
            times.iter().for_each(|(typ, time)| println!("Max time for instruction type {typ}: {time}ms"));
        }

        Ok(())
    }

    pub fn to_binary(&self, file_path: &str, program: &'a Program) -> std::io::Result<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        let time = Instant::now();

        for (_, inst) in program.iter() {
            f.write_all(&inst.as_bytes())?;
        }

        if DEBUG {
            let elapsed = time.elapsed().as_micros();
            println!("Compiling to binary took: {elapsed}ms");
        }

        Ok(())
    }

    pub fn from_binary(file_path: &'a str, buf: &'a Vec::<u8>, natives: Natives, _: Vec::<&'a str>) -> MResult::<'a, (Mm<'a>, Program)> {
        let time = Instant::now();

        let mut i = 0;
        let mut ip = 0;
        let mut labels = Labels::with_capacity(10);
        let mut program = Program::with_capacity(50);
        while i < buf.len() {
            let (inst, size) = Inst::from_bytes(&buf[i..])?;
            match inst.typ {
                InstType::LABEL => { labels.insert(inst.val.as_string().to_owned(), ip); }
                _ => {}
            };
            ip += 1;
            i += size;
            program.push(((68, 69), inst));
        }

        let Some(entry_point) = labels.iter().find(|(l, _)| l == &&ENTRY_POINT).map(|(_, i)| *i) else {
            return Err(Trap::NoEntryPointFound(file_path))
        };

        if matches!(program.last(), Some(last) if last.1.typ != InstType::HALT) {
            let inst = Inst { typ: InstType::HALT, val: InstValue::None };
            program.push(((68, 69), inst));
        }

        if DEBUG {
            let elapsed = time.elapsed().as_micros();
            println!("Compiling from binary took: {elapsed}ms");
        }

        // let libs = lib_paths.iter().map(|l| load_lib(l).unwrap()).collect::<Vec::<_>>();
        // let externs = Mm::process_externs(&program, &libs);
        let externs = Externs::new();
        Mm::check_natives(&program, &natives).unwrap();

        let mm = Mm {
            file_path,
            stack: VecDeque::with_capacity(1024),
            call_stack: VecDeque::with_capacity(Mm::CALL_STACK_CAP),
            memory: [0; MEMORY_CAP],
            natives,
            externs,
            labels,
            flags: Flags::new(),
            ip: entry_point,
            halt: false,
        };

        Ok((mm, program))
    }

    pub fn generate_masm(&self, file_path: &str, program: &'a Program) -> std::io::Result<()> {
        use std::{fs::File, io::Write};

        let mut f = File::create(file_path)?;
        let time = Instant::now();

        for (_, inst) in program.iter() {
            let inst_str = format!("{inst}\n", inst = String::from(inst));
            f.write_all(&inst_str.as_bytes())?;
        }

        if DEBUG {
            let elapsed = time.elapsed().as_micros();
            println!("Generation took: {elapsed} microseconds");
        }

        Ok(())
    }
}

/* TODO:
    (#12) Implement proper errors and do not just `panic!`, even more embed lexer into the VM, to get even better error messages.
    (#13) Allow use of macros inside of macros.
    (#14) Introduce notes to errors.

    1. Use lifetimes to get rid of cloning values instead of taking reference.
*/

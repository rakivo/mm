use crate::{Inst, Type};

#[derive(Clone)]
pub enum Trap {
    StackOverflow(Inst),
    StackUnderflow(Inst),
    CallStackOverflow(Inst),
    CallStackUnderflow(Inst),
    DivisionByZero(Inst),
    IllegalInstructionAccess,
    NoEntryPointFound(String),
    InvalidOperand(String, Option::<String>),
    UndefinedSymbol(String),
    InvalidLabel(String, String),
    InvalidFunction(String, String),
    DivisionOfDifferentTypes(Result::<Type, ()>, Result::<Type, ()>)
}

impl std::fmt::Debug for Trap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Trap::*;
        match self {
            StackOverflow(inst)                => write!(f, "Stack overflow, Last executed {inst}"),
            StackUnderflow(inst)               => write!(f, "Stack underflow, Last executed {inst}"),
            CallStackOverflow(inst)            => write!(f, "Call stack overflow, Last executed {inst}"),
            CallStackUnderflow(inst)           => write!(f, "Call stack underflow, Last executed {inst}"),
            DivisionByZero(inst)               => write!(f, "Division by zero, Last executed {inst}"),
            UndefinedSymbol(sym)               => write!(f, "Undefined symbol: {sym}"),
            InvalidOperand(inst, oper)         => if let Some(oper) = oper {
                write!(f, "Invalid operand, Last executed instruction: {inst}, {oper}")
            } else { write!(f, "Invalid operand, Last executed instruction: {inst}") }
            InvalidLabel(label, reason)        => write!(f, "Invalid label: `{label}`: {reason}"),
            InvalidFunction(func, reason)      => write!(f, "Invalid function: `{func}`: {reason}"),
            NoEntryPointFound(file_path)       => write!(f, "No entry point found in: {file_path}"),
            IllegalInstructionAccess           => write!(f, "Illegal instruction access"),
            DivisionOfDifferentTypes(ty1, ty2) => write!(f, "Can't divide different types, type 1: {ty1:?}, type 2: {ty2:?}")
        }
    }
}

pub struct MTrap(pub Trap, pub Option::<(String, usize)>);

impl From::<(&str, usize, Trap)> for MTrap {
    fn from(t: (&str, usize, Trap)) -> Self {
        let file_path = t.0;
        let row = t.1;
        let trap = t.2;
        MTrap(trap, Some((file_path.to_owned(), row)))
    }
}

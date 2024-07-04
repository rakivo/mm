use std::borrow::Cow;
use crate::{Inst, Loc, InstType, Type};

#[derive(Clone)]
pub enum Trap {
    StackOverflow(InstType),
    StackUnderflow(InstType),
    CallStackOverflow(Inst),
    CallStackUnderflow(Inst),
    DivisionByZero(InstType),
    IllegalInstructionAccess,
    NoEntryPointFound(String),
    InvalidOperand(String, Option::<String>),
    UndefinedSymbol(String),
    InvalidLabel(String, String),
    InvalidFunction(String, String),
    OperationWithDifferentTypes(Result::<Type, ()>, Result::<Type, ()>)
}

impl std::fmt::Debug for Trap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Trap::*;
        match self {
            StackOverflow(inst)                => write!(f, "Stack overflow, Last executed {inst:?}"),
            StackUnderflow(inst)               => write!(f, "Stack underflow, Last executed {inst:?}"),
            CallStackOverflow(inst)            => write!(f, "Call stack overflow, Last executed {inst}"),
            CallStackUnderflow(inst)           => write!(f, "Call stack underflow, Last executed {inst}"),
            DivisionByZero(inst)               => write!(f, "Division by zero, Last executed {inst:?}"),
            UndefinedSymbol(sym)               => write!(f, "Undefined symbol: {sym}"),
            InvalidOperand(inst, oper)         => if let Some(oper) = oper {
                write!(f, "Invalid operand, Last executed instruction: {inst}, {oper}")
            } else { write!(f, "Invalid operand, Last executed instruction: {inst}") }
            InvalidLabel(label, reason)        => write!(f, "Invalid label: `{label}`: {reason}"),
            InvalidFunction(func, reason)      => write!(f, "Invalid function: `{func}`: {reason}"),
            NoEntryPointFound(file_path)       => write!(f, "No entry point found in: {file_path}"),
            IllegalInstructionAccess           => write!(f, "Illegal instruction access"),
            OperationWithDifferentTypes(ty1, ty2) => write!(f, "Can't perform an operation with two different types, type 1: {ty1:?}, type 2: {ty2:?}")
        }
    }
}

pub struct MTrap<'a>(pub Cow<'a, str>, pub Loc, pub Trap);

// Adding 1 to the row to convert it from 0-based indexing
impl std::fmt::Debug for MTrap<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (file_path, loc, trap) = (&self.0, &self.1, &self.2);
        let (row, col) = (loc.0 + 1, loc.1);
        write!(f, "{file_path}:{row}:{col}: {trap:?}")
    }
}

impl<'a> From::<(Cow<'a, str>, Loc, Trap)> for MTrap<'a> {
    fn from(t: (Cow<'a, str>, Loc, Trap)) -> Self {
        let (file_path, loc, trap) = (t.0, t.1, t.2);
        MTrap(file_path, loc, trap)
    }
}

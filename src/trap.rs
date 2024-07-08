use std::borrow::Cow;
use crate::{NaNBox, Inst, Loc, InstType, Type};

pub enum Trap<'a> {
    InvalidOperand(&'a str),
    UndefinedSymbol(String),
    UndeclaredNative(&'a String),
    StackOverflow(InstType),
    StackUnderflow(InstType),
    CallStackOverflow(&'a Inst),
    CallStackUnderflow(&'a Inst),
    DivisionByZero(InstType),
    IllegalInstructionAccess,
    NoEntryPointFound(&'a str),
    InvalidLabel(&'a str, &'a str),
    InvalidPpType(String, &'a str),
    InvalidType(NaNBox, Type, Type),
    InvalidFunction(&'a str, &'a str),
    FailedConversion(NaNBox, Type, Type),
    OperationWithDifferentTypes(Type, Type)
}

impl std::fmt::Debug for Trap<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Trap::*;
        match self {
            StackOverflow(inst)                   => write!(f, "Stack overflow, Last executed {inst:?}"),
            StackUnderflow(inst)                  => write!(f, "Stack underflow, Last executed {inst:?}"),
            CallStackOverflow(inst)               => write!(f, "Call stack overflow, Last executed {inst}"),
            CallStackUnderflow(inst)              => write!(f, "Call stack underflow, Last executed {inst}"),
            DivisionByZero(inst)                  => write!(f, "Division by zero, Last executed {inst:?}"),
            UndefinedSymbol(sym)                  => write!(f, "Undefined symbol: {sym}"),
            UndeclaredNative(native)              => write!(f, "Undeclared native function: {native}"),
            InvalidOperand(inst)                  => write!(f, "Invalid operand, Last executed instruction: {inst}"),
            InvalidLabel(label, reason)           => write!(f, "Invalid label: `{label}`: {reason}"),
            InvalidFunction(func, reason)         => write!(f, "Invalid function: `{func}`: {reason}"),
            NoEntryPointFound(file_path)          => write!(f, "No entry point found in: {file_path}"),
            IllegalInstructionAccess              => write!(f, "Illegal instruction access"),
            InvalidPpType(of, expected)           => write!(f, "Undefined symbol: {of:?}, expected type: {expected}"),
            FailedConversion(val, t1, t2)         => write!(f, "Failed to convert: {val} from: {t1:?} to: {t2:?}"),
            InvalidType(val, of, expected)        => write!(f, "Expected type: {expected:?}, but got: {of:?}, value: {val}"),
            OperationWithDifferentTypes(ty1, ty2) => write!(f, "Can't perform an operation with two different types, type 1: {ty1:?}, type 2: {ty2:?}")
        }
    }
}

pub struct MTrap<'a>(pub Cow<'a, str>, pub Loc, pub Trap<'a>);

impl std::fmt::Debug for MTrap<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (file_path, loc, trap) = (&self.0, &self.1, &self.2);
        // Adding 1 to the row to convert it from 0-based indexing
        let (row, col) = (loc.0 + 1, loc.1);
        write!(f, "{file_path}:{row}:{col}: {trap:?}")
    }
}

impl<'a> From<(Cow<'a, str>, Loc, Trap<'a>)> for MTrap<'a> {
    #[inline]
    fn from(t: (Cow<'a, str>, Loc, Trap<'a>)) -> Self {
        let (file_path, loc, trap) = (t.0, t.1, t.2);
        MTrap(file_path, loc, trap)
    }
}

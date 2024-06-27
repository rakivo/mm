use crate::{Inst, Type};

#[derive(Clone)]
// Inst, operand
pub struct InstString(pub String, pub Option::<String>);

#[derive(Clone)]
pub enum Trap {
    StackOverflow(Inst),
    StackUnderflow(Inst),
    DivisionByZero(Inst),
    IllegalInstructionAccess,
    NoEntryPointFound(String),
    InvalidOperand(InstString),
    InvalidLabel(String, String),
    IllegalInstruction(Option::<String>),
    DivisionOfDifferentTypes(Result::<Type, ()>, Result::<Type, ()>)
}

impl std::fmt::Display for InstString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ref inst = self.0;
        if let Some(ref oper) = self.1 {
            write!(f, "Instruction: {inst}, operand: {oper}")
        } else {
            write!(f, "Instruction: {inst}")
        }
    }
}

impl std::fmt::Debug for Trap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Trap::*;
        match self {
            StackOverflow(inst)                => write!(f, "Stack overflow, Last executed {inst}"),
            StackUnderflow(inst)               => write!(f, "Stack underflow, Last executed {inst}"),
            DivisionByZero(inst)               => write!(f, "Division by zero, Last executed {inst}"),
            InvalidOperand(inst)               => write!(f, "Invalid operand, Last executed {inst}"),
            InvalidLabel(label, reason)        => write!(f, "Invalid label: `{label}`: {reason}"),
            IllegalInstruction(inst_opt)       => if let Some(inst) = inst_opt {
                write!(f, "Illegal instruction: {inst}")
            } else { write!(f, "Illegal instruction") }
            NoEntryPointFound(file_path)       => write!(f, "No entry point found in: {file_path}"),
            IllegalInstructionAccess           => write!(f, "Illegal instruction access"),
            DivisionOfDifferentTypes(ty1, ty2) => write!(f, "Can't divide different types, type 1: {ty1:?}, type 2: {ty2:?}")
        }
    }
}

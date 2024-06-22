use crate::Inst;

#[derive(Clone)]
pub struct InstString(pub String, pub Option::<String>);

#[derive(Clone)]
pub enum Trap {
    StackOverflow(Inst),
    StackUnderflow(Inst),
    DivisionByZero(Inst),
    InvalidOperand(InstString),
    InvalidLabel(String),
    IllegalInstruction(Option::<String>),
    IllegalInstructionAccess
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
            StackOverflow(inst)          => write!(f, "ERROR: Stack overflow, Last executed: {inst}"),
            StackUnderflow(inst)         => write!(f, "ERROR: Stack underflow, Last executed: {inst}"),
            DivisionByZero(inst)         => write!(f, "ERROR: Division by zero, Last executed: {inst}"),
            InvalidOperand(inst)         => write!(f, "ERROR: Invalid operand, Last executed: {inst}"),
            InvalidLabel(label)          => write!(f, "ERROR: Invalid label: {label}"),
            IllegalInstruction(inst_opt) => if let Some(inst) = inst_opt {
                write!(f, "ERROR: Illegal instruction: {inst}")
            } else {
                write!(f, "ERROR: Illegal instruction")
            }
            IllegalInstructionAccess     => write!(f, "Illegal instruction access"),
        }
    }
}

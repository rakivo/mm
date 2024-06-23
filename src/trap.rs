use crate::Inst;

#[derive(Clone)]
pub struct InstString(pub String, pub Option::<String>);

impl From::<Inst> for InstString {
    fn from(inst: Inst) -> Self {
        use Inst::*;
        match inst {
            NOP         => InstString("nop".to_owned(), None),
            PUSH(oper)  => InstString("push".to_owned(), Some(oper.to_string())),
            POP         => InstString("pop".to_owned(), None),
            INC         => InstString("inc".to_owned(), None),
            DEC         => InstString("dec".to_owned(), None),
            ADD         => InstString("add".to_owned(), None),
            SUB         => InstString("sub".to_owned(), None),
            MUL         => InstString("mul".to_owned(), None),
            DIV         => InstString("div".to_owned(), None),
            CMP(oper)   => InstString("cmp".to_owned(), Some(oper.to_string())),
            SWAP        => InstString("swap".to_owned(), None),
            DUP(oper)   => InstString("dup".to_owned(), Some(oper.to_string())),
            JE(oper)    => InstString("je".to_owned(), Some(oper)),
            JL(oper)    => InstString("jl".to_owned(), Some(oper)),
            JNGE(oper)  => InstString("jnge".to_owned(), Some(oper)),
            JG(oper)    => InstString("jg".to_owned(), Some(oper)),
            JNLE(oper)  => InstString("jnle".to_owned(), Some(oper)),
            JNE(oper)   => InstString("jne".to_owned(), Some(oper)),
            JZ(oper)    => InstString("jz".to_owned(), Some(oper)),
            JNZ(oper)   => InstString("jnz".to_owned(), Some(oper)),
            JMP(oper)   => InstString("jmp".to_owned(), Some(oper)),
            LABEL(oper) => InstString(oper, None),
            BOT         => InstString("bot".to_owned(), None),
            HALT        => InstString("halt".to_owned(), None)
        }
    }
}

#[derive(Clone)]
pub enum Trap {
    StackOverflow(Inst),
    StackUnderflow(Inst),
    DivisionByZero(Inst),
    InvalidOperand(InstString),
    InvalidLabel(String, String),
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
            StackOverflow(inst)          => write!(f, "ERROR: Stack overflow, Last executed {inst}"),
            StackUnderflow(inst)         => write!(f, "ERROR: Stack underflow, Last executed {inst}"),
            DivisionByZero(inst)         => write!(f, "ERROR: Division by zero, Last executed {inst}"),
            InvalidOperand(inst)         => write!(f, "ERROR: Invalid operand, Last executed {inst}"),
            InvalidLabel(label, reason)  => write!(f, "ERROR: Invalid label: `{label}`: {reason}"),
            IllegalInstruction(inst_opt) => if let Some(inst) = inst_opt {
                write!(f, "ERROR: Illegal instruction: {inst}")
            } else {
                write!(f, "ERROR: Illegal instruction")
            }
            IllegalInstructionAccess     => write!(f, "Illegal instruction access"),
        }
    }
}

use crate::{Word, Trap};

#[derive(Debug)]
pub enum Inst {
    NOP,
    PUSH(Word),
    POP,
    ADD,
    SUB,
    MUL,
    DIV,
    CMP,
    SWAP,
    DUP(usize),
    JE(usize),
    JL(usize),
    JNGE(usize),
    JG(usize),
    JNLE(usize),
    JZ(usize),
    JNZ(usize),
    JMP(usize),
    HALT
}

macro_rules! extend_from_byte {
    ($a: expr, $ret: expr) => {{
        let mut bytes = vec![$a];
        bytes.extend(&$ret.to_le_bytes());
        bytes
    }};
}

macro_rules! inst_from_bytes {
    ($b: ident, $ret: tt, $ty: ty) => {
        if $b.len() >= 9 {
            let mut array = [0; 8];
            array.copy_from_slice(&$b[1..9]);
            let a = usize::from_le_bytes(array);
            Ok((Inst::$ret(a as $ty), 9))
        } else { Err(Trap::InvalidOperand) }
    }
}

impl Inst {
    pub fn to_bytes(&self) -> Vec::<u8> {
        match self {
            Inst::NOP        => vec![0],
            Inst::PUSH(val)  => extend_from_byte!(1, *val),
            Inst::POP        => vec![2],
            Inst::ADD        => vec![3],
            Inst::SUB        => vec![4],
            Inst::MUL        => vec![5],
            Inst::DIV        => vec![6],
            Inst::CMP        => vec![7],
            Inst::SWAP       => vec![8],
            Inst::DUP(val)   => extend_from_byte!(9, *val),
            Inst::JE(addr)   => extend_from_byte!(10, *addr),
            Inst::JL(addr)   => extend_from_byte!(11, *addr),
            Inst::JNGE(addr) => extend_from_byte!(12, *addr),
            Inst::JG(addr)   => extend_from_byte!(13, *addr),
            Inst::JNLE(addr) => extend_from_byte!(14, *addr),
            Inst::JZ(addr)   => extend_from_byte!(15, *addr),
            Inst::JNZ(addr)  => extend_from_byte!(16, *addr),
            Inst::JMP(addr)  => extend_from_byte!(17, *addr),
            Inst::HALT       => vec![18],
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Inst, usize), Trap> {
        match bytes.get(0) {
            Some(0)  => Ok((Inst::NOP, 1)),
            Some(1)  => inst_from_bytes!(bytes, PUSH, Word),
            Some(2)  => Ok((Inst::POP, 1)),
            Some(3)  => Ok((Inst::ADD, 1)),
            Some(4)  => Ok((Inst::SUB, 1)),
            Some(5)  => Ok((Inst::MUL, 1)),
            Some(6)  => Ok((Inst::DIV, 1)),
            Some(7)  => Ok((Inst::CMP, 1)),
            Some(8)  => Ok((Inst::SWAP, 1)),
            Some(9)  => inst_from_bytes!(bytes, DUP, usize),
            Some(10) => inst_from_bytes!(bytes, JE, usize),
            Some(11) => inst_from_bytes!(bytes, JL, usize),
            Some(12) => inst_from_bytes!(bytes, JNGE, usize),
            Some(13) => inst_from_bytes!(bytes, JG, usize),
            Some(14) => inst_from_bytes!(bytes, JNLE, usize),
            Some(15) => inst_from_bytes!(bytes, JZ, usize),
            Some(16) => inst_from_bytes!(bytes, JNZ, usize),
            Some(17) => inst_from_bytes!(bytes, JMP, usize),
            Some(18) => Ok((Inst::HALT, 1)),
            _        => Err(Trap::IllegalInstruction),
        }
    }
}

impl std::convert::TryFrom<&str> for Inst {
    type Error = Trap;

    fn try_from(s: &str) -> Result<Inst, Self::Error> {
        let mut splitted = s.split_whitespace();
        let inst = splitted.next().ok_or(Trap::IllegalInstruction)?;
        let oper = if let Some(oper) = splitted.next() {
            Some(oper.parse::<Word>().map_err(|_| Trap::InvalidOperand)?)
        } else { None };
        match inst {
            "nop"  => Ok(Inst::NOP),
            "pop"  => Ok(Inst::POP),
            "add"  => Ok(Inst::ADD),
            "sub"  => Ok(Inst::SUB),
            "mul"  => Ok(Inst::MUL),
            "div"  => Ok(Inst::DIV),
            "cmp"  => Ok(Inst::CMP),
            "halt" => Ok(Inst::HALT),
            "swap" => Ok(Inst::SWAP),
            "push" => Ok(Inst::PUSH(oper.ok_or(Trap::InvalidOperand)?)),
            "dup"  => Ok(Inst::DUP(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "je"   => Ok(Inst::JE(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jl"   => Ok(Inst::JL(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jnge" => Ok(Inst::JNGE(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jg"   => Ok(Inst::JG(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jnle" => Ok(Inst::JNLE(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jz"   => Ok(Inst::JZ(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jnz"  => Ok(Inst::JNZ(oper.ok_or(Trap::InvalidOperand)? as usize)),
            "jmp"  => Ok(Inst::JMP(oper.ok_or(Trap::InvalidOperand)? as usize)),
            _      => Err(Trap::IllegalInstruction)
        }
    }
}

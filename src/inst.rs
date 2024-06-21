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
    DUP(Word),
    JE(Word),
    JL(Word),
    JNGE(Word),
    JG(Word),
    JNLE(Word),
    JZ(Word),
    JNZ(Word),
    JMP(Word),
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
    ($b: ident, $ret: tt) => {{
        if $b.len() >= 9 {
            let mut array = [0; 8];
            array.copy_from_slice(&$b[1..9]);
            let a = Word::from_le_bytes(array);
            Ok((Inst::$ret(a), 9))
        } else { Err(Trap::InvalidOperand(None)) }
    }}
}

impl Inst {
    pub fn as_bytes(&self) -> Vec::<u8> {
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
            Some(1)  => inst_from_bytes!(bytes, PUSH),
            Some(2)  => Ok((Inst::POP, 1)),
            Some(3)  => Ok((Inst::ADD, 1)),
            Some(4)  => Ok((Inst::SUB, 1)),
            Some(5)  => Ok((Inst::MUL, 1)),
            Some(6)  => Ok((Inst::DIV, 1)),
            Some(7)  => Ok((Inst::CMP, 1)),
            Some(8)  => Ok((Inst::SWAP, 1)),
            Some(9)  => inst_from_bytes!(bytes, DUP),
            Some(10) => inst_from_bytes!(bytes, JE),
            Some(11) => inst_from_bytes!(bytes, JL),
            Some(12) => inst_from_bytes!(bytes, JNGE),
            Some(13) => inst_from_bytes!(bytes, JG),
            Some(14) => inst_from_bytes!(bytes, JNLE),
            Some(15) => inst_from_bytes!(bytes, JZ),
            Some(16) => inst_from_bytes!(bytes, JNZ),
            Some(17) => inst_from_bytes!(bytes, JMP),
            Some(18) => Ok((Inst::HALT, 1)),
            _        => Err(Trap::IllegalInstruction(Some(String::from_utf8_lossy(bytes).to_string()))),
        }
    }
}

impl std::convert::TryFrom<&str> for Inst {
    type Error = Trap;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let mut splitted = s.split_whitespace();

        let inst_string = splitted.next();
        let inst_err = Trap::IllegalInstruction(inst_string.map(|s| s.to_owned()));
        let inst = inst_string.ok_or(inst_err.to_owned())?;

        let oper_string = splitted.next();
        let oper_err = Trap::InvalidOperand(oper_string.map(|s| s.to_owned()));
        let oper = if let Some(ref oper) = oper_string {
            Some(oper.parse::<Word>().map_err(|_| oper_err.to_owned())?)
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
            "push" => Ok(Inst::PUSH(oper.ok_or(oper_err.to_owned())?)),
            "dup"  => Ok(Inst::DUP(oper.ok_or(oper_err.to_owned())?)),
            "je"   => Ok(Inst::JE(oper.ok_or(oper_err.to_owned())?)),
            "jl"   => Ok(Inst::JL(oper.ok_or(oper_err.to_owned())?)),
            "jnge" => Ok(Inst::JNGE(oper.ok_or(oper_err.to_owned())?)),
            "jg"   => Ok(Inst::JG(oper.ok_or(oper_err.to_owned())?)),
            "jnle" => Ok(Inst::JNLE(oper.ok_or(oper_err.to_owned())?)),
            "jz"   => Ok(Inst::JZ(oper.ok_or(oper_err.to_owned())?)),
            "jnz"  => Ok(Inst::JNZ(oper.ok_or(oper_err.to_owned())?)),
            "jmp"  => Ok(Inst::JMP(oper.ok_or(oper_err)?)),
            _      => Err(inst_err)
        }
    }
}

impl From<&Inst> for String {
    fn from(inst: &Inst) -> Self {
        use Inst::*;
        match inst {
            NOP        => format!("nop"),
            PUSH(oper) => format!("push    {oper}"),
            POP        => format!("pop"),
            ADD        => format!("add"),
            SUB        => format!("sub"),
            MUL        => format!("mul"),
            DIV        => format!("div"),
            CMP        => format!("cmp"),
            SWAP       => format!("swap"),
            DUP(oper)  => format!("dup     {oper}"),
            JE(oper)   => format!("je      {oper}"),
            JL(oper)   => format!("jl      {oper}"),
            JNGE(oper) => format!("jnge    {oper}"),
            JG(oper)   => format!("jg      {oper}"),
            JNLE(oper) => format!("jnle    {oper}"),
            JZ(oper)   => format!("jz      {oper}"),
            JNZ(oper)  => format!("jnz     {oper}"),
            JMP(oper)  => format!("jmp     {oper}"),
            HALT       => format!("halt"),
        }
    }
}

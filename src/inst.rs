use crate::{Word, Trap, InstString};

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    NOP,
    PUSH(Word),
    POP,
    INC,
    DEC,
    ADD,
    SUB,
    MUL,
    DIV,
    CMP(Word),
    SWAP,
    DUP(Word),

    JE(String),
    JL(String),
    JNGE(String),
    JG(String),
    JNLE(String),
    JNE(String),
    JZ(String),
    JNZ(String),
    JMP(String),
    LABEL(String),

    BOT,

    HALT,
}

macro_rules! extend_from_byte {
    (i.$a: expr, $ret: expr) => {{
        let mut bytes = vec![$a];
        bytes.extend(&$ret.to_le_bytes());
        bytes
    }};
    ($a: expr, $ret: expr) => {{
        let mut bytes = vec![$a];
        let str_bytes = $ret.as_bytes();
        let str_len = str_bytes.len() as u64;
        bytes.extend(&str_len.to_le_bytes());
        bytes.extend(str_bytes);
        bytes
    }}
}

macro_rules! inst_from_bytes {
    (i.$b: ident, $ret: tt) => {{
        if $b.len() >= 9 {
            let mut array = [0; 8];
            array.copy_from_slice(&$b[1..9]);
            let a = Word::from_le_bytes(array);
            Ok((Inst::$ret(a), 9))
        } else {
            Err(Trap::InvalidOperand(InstString(stringify!($ret).to_owned(), None)))
        }
    }};
    ($b:ident, $ret:tt) => {{
        if $b.len() >= 9 {
            let mut len_array = [0; 8];
            len_array.copy_from_slice(&$b[1..9]);
            let str_len = u64::from_le_bytes(len_array) as usize;
            if $b.len() >= 9 + str_len {
                let str_bytes = &$b[9..9 + str_len];
                let a = String::from_utf8_lossy(str_bytes).to_string();
                Ok((Inst::$ret(a), 9 + str_len))
            } else {
                Err(Trap::InvalidOperand(InstString(stringify!($ret).to_owned(), None)))
            }
        } else {
            Err(Trap::InvalidOperand(InstString(stringify!($ret).to_owned(), None)))
        }
    }}
}

impl Inst {
    pub fn as_bytes(&self) -> Vec::<u8> {
        use Inst::*;
        match self {
            NOP         => vec![0],
            PUSH(val)   => extend_from_byte!(i.1, *val),
            POP         => vec![2],
            INC         => vec![3],
            DEC         => vec![4],
            ADD         => vec![5],
            SUB         => vec![6],
            MUL         => vec![7],
            DIV         => vec![8],
            CMP(val)    => extend_from_byte!(i.9, *val),
            SWAP        => vec![10],
            DUP(val)    => extend_from_byte!(i.11, *val),
            JE(addr)    => extend_from_byte!(12, *addr),
            JL(addr)    => extend_from_byte!(13, *addr),
            JNGE(addr)  => extend_from_byte!(14, *addr),
            JG(addr)    => extend_from_byte!(15, *addr),
            JNLE(addr)  => extend_from_byte!(16, *addr),
            JNE(addr)   => extend_from_byte!(17, *addr),
            JZ(addr)    => extend_from_byte!(18, *addr),
            JNZ(addr)   => extend_from_byte!(19, *addr),
            JMP(addr)   => extend_from_byte!(20, *addr),
            LABEL(addr) => extend_from_byte!(21, *addr),
            BOT         => vec![22],
            HALT        => vec![69],
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Inst, usize), Trap> {
        use Inst::*;
        match bytes.first() {
            Some(0)  => Ok((NOP, 1)),
            Some(1)  => inst_from_bytes!(i.bytes, PUSH),
            Some(2)  => Ok((POP, 1)),
            Some(3)  => Ok((INC, 1)),
            Some(4)  => Ok((DEC, 1)),
            Some(5)  => Ok((ADD, 1)),
            Some(6)  => Ok((SUB, 1)),
            Some(7)  => Ok((MUL, 1)),
            Some(8)  => Ok((DIV, 1)),
            Some(9)  => inst_from_bytes!(i.bytes, CMP),
            Some(10) => Ok((SWAP, 1)),
            Some(11) => inst_from_bytes!(i.bytes, DUP),
            Some(12) => inst_from_bytes!(bytes, JE),
            Some(13) => inst_from_bytes!(bytes, JL),
            Some(14) => inst_from_bytes!(bytes, JNGE),
            Some(15) => inst_from_bytes!(bytes, JG),
            Some(16) => inst_from_bytes!(bytes, JNLE),
            Some(17) => inst_from_bytes!(bytes, JNE),
            Some(18) => inst_from_bytes!(bytes, JZ),
            Some(19) => inst_from_bytes!(bytes, JNZ),
            Some(20) => inst_from_bytes!(bytes, JMP),
            Some(21) => inst_from_bytes!(bytes, LABEL),
            Some(22) => Ok((BOT, 1)),
            Some(69) => Ok((HALT, 1)),
            _        => Err(Trap::IllegalInstruction(Some(String::from_utf8_lossy(bytes).to_string()))),
        }
    }
}

impl std::convert::TryFrom<&str> for Inst {
    type Error = Trap;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        use Inst::*;

        let mut splitted = s.split_whitespace();

        let inst_string = splitted.next();
        let inst_err = Trap::IllegalInstruction(inst_string.map(|s| s.to_owned()));
        let inst = inst_string.ok_or(inst_err.to_owned())?;

        let oper = splitted.next().map(|s| s.to_owned());
        let oper_err = Trap::InvalidOperand(InstString(inst.to_owned(), oper.to_owned()));

        let parse_word = || -> Result<Word, Self::Error> {
            oper.to_owned().ok_or(oper_err.to_owned())?.parse::<Word>().map_err(|_| oper_err.clone())
        };

        let get_oper = || -> Result::<String, Self::Error> {
            oper.to_owned().ok_or(oper_err.to_owned())
        };

        if matches!(inst.chars().next(), Some(ch) if ch.is_ascii()) && inst.ends_with(':') {
            return Ok(LABEL(inst[..inst.len() - 1].to_owned()))
        }

        match inst {
            "nop"  => Ok(NOP),
            "pop"  => Ok(POP),
            "inc"  => Ok(INC),
            "dec"  => Ok(DEC),
            "add"  => Ok(ADD),
            "sub"  => Ok(SUB),
            "mul"  => Ok(MUL),
            "div"  => Ok(DIV),
            "cmp"  => Ok(CMP(parse_word()?)),
            "halt" => Ok(HALT),
            "swap" => Ok(SWAP),
            "push" => Ok(PUSH(parse_word()?)),
            "dup"  => Ok(DUP(parse_word()?)),
            "je" | "jl" | "jnge" | "jg" | "jnle" | "jne" | "jz" | "jnz" | "jmp" => {
                if parse_word().is_err() {
                    match inst {
                        "je"   => Ok(JE(get_oper()?)),
                        "jl"   => Ok(JL(get_oper()?)),
                        "jg"   => Ok(JG(get_oper()?)),
                        "jnge" => Ok(JNGE(get_oper()?)),
                        "jnle" => Ok(JNLE(get_oper()?)),
                        "jne"  => Ok(JNE(get_oper()?)),
                        "jz"   => Ok(JZ(get_oper()?)),
                        "jnz"  => Ok(JNZ(get_oper()?)),
                        "jmp"  => Ok(JMP(get_oper()?)),
                        _ => Err(inst_err),
                    }
                } else {
                    Err(oper_err)
                }
            }
            "bot" => Ok(BOT),
            _ => Err(inst_err)
        }
    }
}

impl From<&Inst> for String {
    fn from(inst: &Inst) -> Self {
        use Inst::*;
        match inst {
            NOP         => format!("nop"),
            PUSH(oper)  => format!("push    {oper}"),
            POP         => format!("pop"),
            INC         => format!("inc"),
            DEC         => format!("dec"),
            ADD         => format!("add"),
            SUB         => format!("sub"),
            MUL         => format!("mul"),
            DIV         => format!("div"),
            CMP(oper)   => format!("cmp     {oper}"),
            SWAP        => format!("swap"),
            DUP(oper)   => format!("dup     {oper}"),
            JE(oper)    => format!("je      {oper}"),
            JL(oper)    => format!("jl      {oper}"),
            JNGE(oper)  => format!("jnge    {oper}"),
            JG(oper)    => format!("jg      {oper}"),
            JNLE(oper)  => format!("jnle    {oper}"),
            JNE(oper)   => format!("jne     {oper}"),
            JZ(oper)    => format!("jz      {oper}"),
            JNZ(oper)   => format!("jnz     {oper}"),
            JMP(oper)   => format!("jmp     {oper}"),
            LABEL(oper) => format!("{oper}:"),
            BOT         => format!("bot"),
            HALT        => format!("halt"),
        }
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Inst::*;
        match self {
            NOP         => write!(f, "Instruction: `NOP`"),
            PUSH(oper)  => write!(f, "Instruction: `PUSH`, operand: `{oper}`"),
            POP         => write!(f, "Instruction: `POP`"),
            INC         => write!(f, "Instruction: `INC`"),
            DEC         => write!(f, "Instruction: `DEC`"),
            ADD         => write!(f, "Instruction: `ADD`"),
            SUB         => write!(f, "Instruction: `SUB`"),
            MUL         => write!(f, "Instruction: `MUL`"),
            DIV         => write!(f, "Instruction: `DIV`"),
            CMP(oper)   => write!(f, "Instruction: `CMP`, operand: `{oper}`"),
            SWAP        => write!(f, "Instruction: `SWAP`"),
            DUP(oper)   => write!(f, "Instruction: `DUP`, operand: `{oper}`"),
            JE(oper)    => write!(f, "Instruction: `JE`, operand: `{oper}`"),
            JL(oper)    => write!(f, "Instruction: `JL`, operand: `{oper}`"),
            JNGE(oper)  => write!(f, "Instruction: `JNGE`, operand: `{oper}`"),
            JG(oper)    => write!(f, "Instruction: `JG`, operand: `{oper}`"),
            JNLE(oper)  => write!(f, "Instruction: `JNLE`, operand: `{oper}`"),
            JNE(oper)   => write!(f, "Instruction: `JNE`, operand: `{oper}`"),
            JZ(oper)    => write!(f, "Instruction: `JZ`, operand: `{oper}`"),
            JNZ(oper)   => write!(f, "Instruction: `JNZ`, operand: `{oper}`"),
            JMP(oper)   => write!(f, "Instruction: `JMP`, operand: `{oper}`"),
            LABEL(oper) => write!(f, "Instruction: `LABEL`, operand: `{oper}`"),
            BOT         => write!(f, "Instruction: `BOT`"),
            HALT        => write!(f, "Instruction: `HALT`"),
        }
    }
}

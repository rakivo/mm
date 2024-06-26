use crate::{Word, Trap, InstString};

#[derive(Clone, Debug)]
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

const MAX_STR_LEN: usize = 16 * 8;
const INSTRUCTION_SIZE: usize = 1 * 8;

fn extend_from_bytes_word(n: u8, val: Word) -> &'static [u8] {
    let mut bytes = [0u8; INSTRUCTION_SIZE + 1];
    bytes[0] = n;
    let le_bytes = unsafe { &val.as_u64 }.to_le_bytes();
    assert!(le_bytes.len() == 8);
    bytes[1..].copy_from_slice(&le_bytes);
    unsafe { std::slice::from_raw_parts(bytes.as_ptr(), INSTRUCTION_SIZE + 1) }
}

fn extend_from_bytes_string(n: u8, val: &str) -> &'static [u8] {
    let mut bytes = [0u8; INSTRUCTION_SIZE + MAX_STR_LEN + 1];
    bytes[0] = n;
    let str_bytes = val.as_bytes();
    let str_len = str_bytes.len();
    assert!(str_len <= MAX_STR_LEN, "Ohh shoot mate, your string is too long, maximum string length: {MAX_STR_LEN}");
    let part_one_end = INSTRUCTION_SIZE + 1;
    let le_bytes = (str_len as u64).to_le_bytes();
    assert!(le_bytes.len() == 8);
    bytes[1..part_one_end].copy_from_slice(&le_bytes);
    bytes[part_one_end..part_one_end + str_len].copy_from_slice(str_bytes);
    unsafe { std::slice::from_raw_parts(bytes.as_ptr(), part_one_end + str_len) }
}

fn word_from_bytes(bytes: &[u8]) -> Word {
    let mut array = [0; 8];
    array.copy_from_slice(&bytes[1..9]);
    Word { as_u64: u64::from_le_bytes(array) }
}

fn string_from_bytes(bytes: &[u8], n: usize) -> String {
    String::from_utf8_lossy(&bytes[9..9 + n]).to_string()
}

macro_rules! inst_from_bytes {
    (i.$b: ident, $ret: tt) => {{
        if $b.len() >= 9 {
            let a = word_from_bytes(&$b);
            Ok((Inst::$ret(a), 9))
        } else {
            Err(Trap::InvalidOperand(InstString(stringify!($ret).to_owned(), None)))
        }
    }};
    ($b:ident, $ret:tt) => {{
        if $b.len() >= 9 {
            let str_len = unsafe { word_from_bytes(&$b).as_u64 } as usize;
            if $b.len() >= 9 + str_len {
                let a = string_from_bytes(&$b, str_len);
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
    pub fn as_bytes(&self) -> &[u8] {
        use Inst::*;
        match self {
            NOP         => &[0],
            PUSH(val)   => extend_from_bytes_word(1, *val),
            POP         => &[2],
            INC         => &[3],
            DEC         => &[4],
            ADD         => &[5],
            SUB         => &[6],
            MUL         => &[7],
            DIV         => &[8],
            CMP(val)    => extend_from_bytes_word(9, *val),
            SWAP        => &[10],
            DUP(val)    => extend_from_bytes_word(11, *val),
            JE(addr)    => extend_from_bytes_string(12, addr),
            JL(addr)    => extend_from_bytes_string(13, addr),
            JNGE(addr)  => extend_from_bytes_string(14, addr),
            JG(addr)    => extend_from_bytes_string(15, addr),
            JNLE(addr)  => extend_from_bytes_string(16, addr),
            JNE(addr)   => extend_from_bytes_string(17, addr),
            JZ(addr)    => extend_from_bytes_string(18, addr),
            JNZ(addr)   => extend_from_bytes_string(19, addr),
            JMP(addr)   => extend_from_bytes_string(20, addr),
            LABEL(addr) => extend_from_bytes_string(21, addr),
            BOT         => &[22],
            HALT        => &[69],
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

impl TryFrom<&str> for Inst {
    type Error = Trap;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        use Inst::*;

        let mut splitted = s.split_whitespace();

        let (inst_err, inst) = {
            let inst_string = splitted.next();
            let inst_err = Trap::IllegalInstruction(inst_string.map(|s| s.to_owned()));
            let inst = inst_string.ok_or(inst_err.to_owned())?;
            (inst_err, inst)
        };

        let (oper_err, oper) = {
            let oper = splitted.next().map(|s| s.to_owned());
            let oper_err = Trap::InvalidOperand(InstString(inst.to_owned(), oper.to_owned()));
            (oper_err, oper)
        };

        let parse_word = || -> Result<Word, Self::Error> {
            oper.to_owned().ok_or(oper_err.to_owned())?.parse::<Word>().map_err(|_| oper_err.to_owned())
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
                        "jnge" => Ok(JNGE(get_oper()?)),
                        "jg"   => Ok(JG(get_oper()?)),
                        "jnle" => Ok(JNLE(get_oper()?)),
                        "jne"  => Ok(JNE(get_oper()?)),
                        "jz"   => Ok(JZ(get_oper()?)),
                        "jnz"  => Ok(JNZ(get_oper()?)),
                        "jmp"  => Ok(JMP(get_oper()?)),
                        _ => unreachable!()
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
        unsafe { match inst {
            NOP         => format!("    nop"),
            PUSH(oper)  => format!("    push    {oper}", oper = oper.as_u64),
            POP         => format!("    pop"),
            INC         => format!("    inc"),
            DEC         => format!("    dec"),
            ADD         => format!("    add"),
            SUB         => format!("    sub"),
            MUL         => format!("    mul"),
            DIV         => format!("    div"),
            CMP(oper)   => format!("    cmp     {oper}", oper = oper.as_u64),
            SWAP        => format!("    swap"),
            DUP(oper)   => format!("    dup     {oper}", oper = oper.as_u64),
            JE(oper)    => format!("    je      {oper}"),
            JL(oper)    => format!("    jl      {oper}"),
            JNGE(oper)  => format!("    jnge    {oper}"),
            JG(oper)    => format!("    jg      {oper}"),
            JNLE(oper)  => format!("    jnle    {oper}"),
            JNE(oper)   => format!("    jne     {oper}"),
            JZ(oper)    => format!("    jz      {oper}"),
            JNZ(oper)   => format!("    jnz     {oper}"),
            JMP(oper)   => format!("    jmp     {oper}"),
            LABEL(oper) => format!("{oper}:"),
            BOT         => format!("    bot"),
            HALT        => format!("    halt"),
        } }
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Inst::*;
        unsafe { match self {
            NOP         => write!(f, "Instruction: `NOP`"),
            PUSH(oper)  => write!(f, "Instruction: `PUSH`, operand: `{oper}`", oper = oper.as_u64),
            POP         => write!(f, "Instruction: `POP`"),
            INC         => write!(f, "Instruction: `INC`"),
            DEC         => write!(f, "Instruction: `DEC`"),
            ADD         => write!(f, "Instruction: `ADD`"),
            SUB         => write!(f, "Instruction: `SUB`"),
            MUL         => write!(f, "Instruction: `MUL`"),
            DIV         => write!(f, "Instruction: `DIV`"),
            CMP(oper)   => write!(f, "Instruction: `CMP`, operand: `{oper}`", oper = oper.as_u64),
            SWAP        => write!(f, "Instruction: `SWAP`"),
            DUP(oper)   => write!(f, "Instruction: `DUP`, operand: `{oper}`", oper = oper.as_u64),
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
        } }
    }
}

impl std::str::FromStr for Word {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value = s.parse::<u64>()?;
        Ok(Word {as_u64: value})
    }
}

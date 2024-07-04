use std::borrow::Cow;
use crate::{NaNBox, Trap, print_oper_f};

#[derive(Clone, Debug, PartialEq)]
pub enum InstType {
    NOP,
    PUSH,
    POP,
    INC,
    DEC,

    IADD,
    ISUB,
    IMUL,
    IDIV,

    FADD,
    FSUB,
    FMUL,
    FDIV,

    CMP,
    SWAP,
    DUP,

    JE,
    JL,
    JNGE,
    JG,
    JNLE,
    JNE,
    JZ,
    JNZ,
    JMP,
    LABEL,

    BOT,
    DMP,

    CALL,

    RET,

    HALT,
}

impl InstType {
    #[inline]
    pub fn is_arg_required(&self) -> bool {
        match self {
              InstType::PUSH
            | InstType::CMP
            | InstType::DUP
            | InstType::JE
            | InstType::JL
            | InstType::JNGE
            | InstType::JG
            | InstType::JNLE
            | InstType::JNE
            | InstType::JZ
            | InstType::JNZ
            | InstType::JMP
            | InstType::LABEL
            | InstType::DMP
            | InstType::CALL => true,
            _ => false
        }
    }
}

impl TryFrom::<&Cow<'_, str>> for InstType {
    type Error = Trap;

    fn try_from(s: &Cow<'_, str>) -> Result::<Self, Self::Error> {
        use InstType::*;
        match s.trim() {
            "nop"   => Ok(NOP),
            "push"  => Ok(PUSH),
            "pop"   => Ok(POP),
            "inc"   => Ok(INC),
            "dec"   => Ok(DEC),
            "iadd"  => Ok(IADD),
            "isub"  => Ok(ISUB),
            "imul"  => Ok(IMUL),
            "idiv"  => Ok(IDIV),
            "fadd"  => Ok(FADD),
            "fsub"  => Ok(FSUB),
            "fmul"  => Ok(FMUL),
            "fdiv"  => Ok(FDIV),
            "cmp"   => Ok(CMP),
            "swap"  => Ok(SWAP),
            "dup"   => Ok(DUP),
            "je"    => Ok(JE),
            "jl"    => Ok(JL),
            "jnge"  => Ok(JNGE),
            "jg"    => Ok(JG),
            "jnle"  => Ok(JNLE),
            "jne"   => Ok(JNE),
            "jz"    => Ok(JZ),
            "jnz"   => Ok(JNZ),
            "jmp"   => Ok(JMP),
            "label" => Ok(LABEL),
            "bot"   => Ok(BOT),
            "dmp"   => Ok(DMP),
            "call"  => Ok(CALL),
            "ret"   => Ok(RET),
            "halt"  => Ok(HALT),
            _       => Err(Trap::UndefinedSymbol(s.to_string()))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InstValue {
    U8(u8),
    F64(NaNBox),
    U64(NaNBox),
    String(String),
    None
}

impl InstValue {
    #[inline]
    pub fn get_u8(&self) -> Option::<u8> {
        match self {
            InstValue::U8(u8_) => Some(*u8_),
            _ => None
        }
    }

    #[inline]
    pub fn get_word(&self) -> Option::<NaNBox> {
        match self {
            InstValue::U64(word) => Some(*word),
            _ => None
        }
    }

    #[inline]
    pub fn get_nan(&self) -> Option::<NaNBox> {
        match self {
            InstValue::F64(nan) => Some(*nan),
            _ => None
        }
    }

    #[inline]
    pub fn get_string(&self) -> Option::<&String> {
        match self {
            InstValue::String(string) => Some(string),
            _ => None
        }
    }

    #[inline]
    #[track_caller]
    pub fn u8_(&self) -> u8 {
        self.get_u8().unwrap()
    }

    #[inline]
    #[track_caller]
    pub fn word(&self) -> NaNBox {
        self.get_word().unwrap()
    }

    #[inline]
    #[track_caller]
    pub fn nan(&self) -> NaNBox {
        self.get_nan().unwrap()
    }

    #[inline]
    #[track_caller]
    pub fn string(&self) -> &String {
        self.get_string().unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Inst {
    pub typ: InstType,
    pub val: InstValue
}

impl TryFrom::<InstType> for Inst {
    type Error = ();

    fn try_from(typ: InstType) -> Result<Self, Self::Error> {
        match typ {
            InstType::NOP  => Ok(Self::NOP),
            InstType::POP  => Ok(Self::POP),
            InstType::INC  => Ok(Self::INC),
            InstType::DEC  => Ok(Self::DEC),
            InstType::IADD => Ok(Self::IADD),
            InstType::ISUB => Ok(Self::ISUB),
            InstType::IMUL => Ok(Self::IMUL),
            InstType::IDIV => Ok(Self::IDIV),
            InstType::FADD => Ok(Self::FADD),
            InstType::FSUB => Ok(Self::FSUB),
            InstType::FMUL => Ok(Self::FMUL),
            InstType::FDIV => Ok(Self::FDIV),
            InstType::SWAP => Ok(Self::SWAP),
            InstType::BOT  => Ok(Self::BOT),
            InstType::RET  => Ok(Self::RET),
            InstType::HALT => Ok(Self::HALT),
            _ => Err(())
        }
    }
}

impl Inst {
    pub const NOP:  Self = Self { typ: InstType::NOP,  val: InstValue::None };
    pub const POP:  Self = Self { typ: InstType::POP,  val: InstValue::None };
    pub const INC:  Self = Self { typ: InstType::INC,  val: InstValue::None };
    pub const DEC:  Self = Self { typ: InstType::DEC,  val: InstValue::None };
    pub const IADD: Self = Self { typ: InstType::IADD, val: InstValue::None };
    pub const ISUB: Self = Self { typ: InstType::ISUB, val: InstValue::None };
    pub const IMUL: Self = Self { typ: InstType::IMUL, val: InstValue::None };
    pub const IDIV: Self = Self { typ: InstType::IDIV, val: InstValue::None };
    pub const FADD: Self = Self { typ: InstType::FADD, val: InstValue::None };
    pub const FSUB: Self = Self { typ: InstType::FSUB, val: InstValue::None };
    pub const FMUL: Self = Self { typ: InstType::FMUL, val: InstValue::None };
    pub const FDIV: Self = Self { typ: InstType::FDIV, val: InstValue::None };
    pub const SWAP: Self = Self { typ: InstType::SWAP, val: InstValue::None };
    pub const BOT:  Self = Self { typ: InstType::BOT,  val: InstValue::None };
    pub const RET:  Self = Self { typ: InstType::RET,  val: InstValue::None };
    pub const HALT: Self = Self { typ: InstType::HALT, val: InstValue::None };

    pub const MAX_STR_LEN: usize = 16 * 8;
    pub const SIZE: usize = 8;

    pub const AS_STR: &'static [&'static str] = &[
        "nop",
        "push",
        "pop",
        "inc",
        "dec",
        "iadd",
        "isub",
        "imul",
        "idiv",
        "fadd",
        "fsub",
        "fmul",
        "fdiv",
        "cmp",
        "swap",
        "dup",
        "je",
        "jl",
        "jnge",
        "jg",
        "jnle",
        "jne",
        "jz",
        "jnz",
        "jmp",
        "label",
        "bot",
        "dmp",
        "call",
        "ret",
        "halt"
    ];
}

const _: () = assert!(std::mem::size_of::<f64>() == Inst::SIZE, "Mm's designed to be working on 64bit");

fn extend_from_bytes_nan(n: u8, val: &NaNBox) -> &'static [u8] {
    let mut bytes = [0u8; Inst::SIZE + 1];
    bytes[0] = n;
    let le_bytes = val.as_f64().to_le_bytes();
    assert!(le_bytes.len() == 8);
    bytes[1..].copy_from_slice(&le_bytes);
    unsafe { std::slice::from_raw_parts(bytes.as_ptr(), Inst::SIZE + 1) }
}

fn extend_to_bytes_string(n: u8, val: &str) -> &'static [u8] {
    let mut bytes = [0u8; Inst::SIZE + Inst::MAX_STR_LEN + 1];
    bytes[0] = n;
    let str_bytes = val.as_bytes();
    let str_len = str_bytes.len();
    assert!(str_len <= Inst::MAX_STR_LEN, "Ohh shoot mate, your string is too long, maximum string length: {m}", m = Inst::MAX_STR_LEN);
    let part_one_end = Inst::SIZE + 1;
    let le_bytes = (str_len as u64).to_le_bytes();
    assert!(le_bytes.len() == 8);
    bytes[1..part_one_end].copy_from_slice(&le_bytes);
    bytes[part_one_end..part_one_end + str_len].copy_from_slice(str_bytes);
    unsafe { std::slice::from_raw_parts(bytes.as_ptr(), part_one_end + str_len) }
}

fn nan_from_bytes(bytes: &[u8]) -> NaNBox {
    let mut array = [0; 8];
    array.copy_from_slice(&bytes[1..9]);
    let f = f64::from_le_bytes(array);
    NaNBox(f)
}

fn string_from_bytes(bytes: &[u8], n: usize) -> String {
    String::from_utf8_lossy(&bytes[9..9 + n]).to_string()
}

macro_rules! inst_from_bytes {
    (i.$b: ident, $ret: tt) => {{
        if $b.len() >= 9 {
            let a = nan_from_bytes(&$b);
            let inst = Inst {
                typ: InstType::$ret,
                val: InstValue::U64(a)
            };

            Ok((inst, 9))
        } else {
            Err(Trap::InvalidOperand(stringify!($ret).to_owned()))
        }
    }};
    ($b: ident, $ret: tt) => {{
        if $b.len() >= 9 {
            let str_len = nan_from_bytes(&$b).as_u64() as usize;
            if $b.len() >= 9 + str_len {
                let a = string_from_bytes(&$b, str_len);
                let inst = Inst {
                    typ: InstType::$ret,
                    val: InstValue::String(a)
                };
                Ok((inst, 9 + str_len))
            } else {
                Err(Trap::InvalidOperand(stringify!($ret).to_owned()))
            }
        } else {
            Err(Trap::InvalidOperand(stringify!($ret).to_owned()))
        }
    }}
}

impl Inst {
    #[inline(always)]
    fn extend_lifetime(fixed: &[u8]) -> &'static [u8] {
        unsafe { std::slice::from_raw_parts(fixed.as_ptr(), fixed.len()) }
    }

    pub fn as_bytes(&self) -> &[u8] {
        use InstType::*;

        match self.typ {
            NOP   => &[0],
            PUSH  => extend_from_bytes_nan(1, &self.val.word()),
            POP   => &[2],
            INC   => &[3],
            DEC   => &[4],

            IADD  => &[5],
            ISUB  => &[6],
            IMUL  => &[7],
            IDIV  => &[8],

            CMP   => extend_from_bytes_nan(9, &self.val.word()),
            SWAP  => &[10],
            DUP   => extend_from_bytes_nan(11, &self.val.word()),
            JE    => extend_to_bytes_string(12, self.val.string()),
            JL    => extend_to_bytes_string(13, self.val.string()),
            JNGE  => extend_to_bytes_string(14, self.val.string()),
            JG    => extend_to_bytes_string(15, self.val.string()),
            JNLE  => extend_to_bytes_string(16, self.val.string()),
            JNE   => extend_to_bytes_string(17, self.val.string()),
            JZ    => extend_to_bytes_string(18, self.val.string()),
            JNZ   => extend_to_bytes_string(19, self.val.string()),
            JMP   => extend_to_bytes_string(20, self.val.string()),
            LABEL => extend_to_bytes_string(21, self.val.string()),
            BOT   => &[22],
            FADD  => &[23],
            FSUB  => &[24],
            FMUL  => &[25],
            FDIV  => &[26],
            DMP   => Self::extend_lifetime(&[27, self.val.u8_()]),
            CALL  => extend_to_bytes_string(28, self.val.string()),
            RET   => &[30],
            HALT  => &[69],
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Inst, usize), Trap> {
        use InstType::*;
        match bytes.first() {
            Some(0)  => Ok((Inst::NOP, 1)),
            Some(1)  => inst_from_bytes!(i.bytes, PUSH),
            Some(2)  => Ok((Inst::POP, 1)),
            Some(3)  => Ok((Inst::INC, 1)),
            Some(4)  => Ok((Inst::DEC, 1)),
            Some(5)  => Ok((Inst::IADD, 1)),
            Some(6)  => Ok((Inst::ISUB, 1)),
            Some(7)  => Ok((Inst::IMUL, 1)),
            Some(8)  => Ok((Inst::IDIV, 1)),
            Some(9)  => inst_from_bytes!(i.bytes, CMP),
            Some(10) => Ok((Inst::SWAP, 1)),
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

            Some(22) => Ok((Inst::BOT, 1)),

            Some(23) => Ok((Inst::FADD, 1)),
            Some(24) => Ok((Inst::FSUB, 1)),
            Some(25) => Ok((Inst::FMUL, 1)),
            Some(26) => Ok((Inst::FDIV, 1)),

            Some(27) => {
                let inst = Inst {
                    typ: DMP,
                    val: if bytes.len() >= 2 {
                        InstValue::U8(bytes[1])
                    } else {
                        return Err(Trap::InvalidOperand("DMP".to_owned()))
                    }
                };

                Ok((inst, 2))
            }

            Some(28) => inst_from_bytes!(bytes, CALL),

            Some(69) => Ok((Inst::HALT, 1)),
            _        => Err(Trap::UndefinedSymbol(String::from_utf8_lossy(bytes).to_string())),
        }
    }
}

impl From<&Inst> for String {
    fn from(inst: &Inst) -> Self {
        let u8_ = || {
            inst.val.get_u8().unwrap()
        };

        let word = || {
            inst.val.get_word().unwrap()
        };

        let string = || {
            inst.val.get_string().unwrap()
        };

        use InstType::*;
        match inst.typ {
            NOP   => format!("    nop"),
            PUSH  => format!("    push    {oper}", oper = word()),
            POP   => format!("    pop"),
            INC   => format!("    inc"),
            DEC   => format!("    dec"),
            IADD  => format!("    iadd"),
            ISUB  => format!("    isub"),
            IMUL  => format!("    imul"),
            IDIV  => format!("    idiv"),
            FADD  => format!("    fadd"),
            FSUB  => format!("    fsub"),
            FMUL  => format!("    fmul"),
            FDIV  => format!("    fdiv"),
            CMP   => format!("    cmp     {oper}", oper = word()),
            SWAP  => format!("    swap"),
            DUP   => format!("    dup     {oper}", oper = word()),
            JE    => format!("    je      {oper}", oper = string()),
            JL    => format!("    jl      {oper}", oper = string()),
            JNGE  => format!("    jnge    {oper}", oper = string()),
            JG    => format!("    jg      {oper}", oper = string()),
            JNLE  => format!("    jnle    {oper}", oper = string()),
            JNE   => format!("    jne     {oper}", oper = string()),
            JZ    => format!("    jz      {oper}", oper = string()),
            JNZ   => format!("    jnz     {oper}", oper = string()),
            JMP   => format!("    jmp     {oper}", oper = string()),
            LABEL => format!("{oper}:", oper = string()),
            CALL  => format!("    call    {oper}", oper = string()),
            BOT   => format!("    bot"),
            RET   => format!("    ret"),
            DMP   => format!("    dmp     {oper}", oper = u8_()),
            HALT  => format!("    halt"),
        }
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use InstType::*;
        match self.typ {
            NOP   => write!(f, "Instruction: `NOP`"),
            PUSH  => { write!(f, "Instruction: `PUSH`, operand: ")?; print_oper_f(f, &self.val.nan()) }
            POP   => write!(f, "Instruction: `POP`"),
            INC   => write!(f, "Instruction: `INC`"),
            DEC   => write!(f, "Instruction: `DEC`"),

            IADD  => write!(f, "Instruction: `IADD`"),
            ISUB  => write!(f, "Instruction: `ISUB`"),
            IMUL  => write!(f, "Instruction: `IMUL`"),
            IDIV  => write!(f, "Instruction: `IDIV`"),

            FADD  => write!(f, "Instruction: `FADD`"),
            FSUB  => write!(f, "Instruction: `FSUB`"),
            FMUL  => write!(f, "Instruction: `FMUL`"),
            FDIV  => write!(f, "Instruction: `FDIV`"),

            CMP   => { write!(f, "Instruction: `CMP`, operand: ")?; print_oper_f(f, &self.val.word()) }
            SWAP  => write!(f, "Instruction: `SWAP`"),
            DUP   => { write!(f, "Instruction: `DUP`, operand: ")?; print_oper_f(f, &self.val.word()) }
            JE    => write!(f, "Instruction: `JE`, operand: `{oper}`", oper = self.val.string()),
            JL    => write!(f, "Instruction: `JL`, operand: `{oper}`", oper = self.val.string()),
            JNGE  => write!(f, "Instruction: `JNGE`, operand: `{oper}`", oper = self.val.string()),
            JG    => write!(f, "Instruction: `JG`, operand: `{oper}`", oper = self.val.string()),
            JNLE  => write!(f, "Instruction: `JNLE`, operand: `{oper}`", oper = self.val.string()),
            JNE   => write!(f, "Instruction: `JNE`, operand: `{oper}`", oper = self.val.string()),
            JZ    => write!(f, "Instruction: `JZ`, operand: `{oper}`", oper = self.val.string()),
            JNZ   => write!(f, "Instruction: `JNZ`, operand: `{oper}`", oper = self.val.string()),
            JMP   => write!(f, "Instruction: `JMP`, operand: `{oper}`", oper = self.val.string()),
            LABEL => write!(f, "Instruction: `LABEL`, operand: `{oper}`", oper = self.val.string()),
            CALL  => write!(f, "Instruction: `CALL`, operand: `{oper}`", oper = self.val.string()),
            BOT   => write!(f, "Instruction: `BOT`"),
            RET   => write!(f, "Instruction: `RET`"),
            DMP   => write!(f, "Instruction: `DMP`, operand: {oper}", oper = self.val.u8_()),
            HALT  => write!(f, "Instruction: `HALT`"),
        }
    }
}

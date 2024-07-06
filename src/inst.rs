use crate::{NaNBox, Trap};

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
    EXTERN,
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
            | InstType::CALL
            | InstType::EXTERN => true,
            _ => false
        }
    }
}

impl std::fmt::Display for InstType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstType::NOP    => write!(f, "nop"),
            InstType::PUSH   => write!(f, "push"),
            InstType::POP    => write!(f, "pop"),
            InstType::INC    => write!(f, "inc"),
            InstType::DEC    => write!(f, "dec"),
            InstType::IADD   => write!(f, "iadd"),
            InstType::ISUB   => write!(f, "isub"),
            InstType::IMUL   => write!(f, "imul"),
            InstType::IDIV   => write!(f, "idiv"),
            InstType::FADD   => write!(f, "fadd"),
            InstType::FSUB   => write!(f, "fsub"),
            InstType::FMUL   => write!(f, "fmul"),
            InstType::FDIV   => write!(f, "fdiv"),
            InstType::CMP    => write!(f, "cmp"),
            InstType::SWAP   => write!(f, "swap"),
            InstType::DUP    => write!(f, "dup"),
            InstType::JE     => write!(f, "je"),
            InstType::JL     => write!(f, "jl"),
            InstType::JNGE   => write!(f, "jnge"),
            InstType::JG     => write!(f, "jg"),
            InstType::JNLE   => write!(f, "jnle"),
            InstType::JNE    => write!(f, "jne"),
            InstType::JZ     => write!(f, "jz"),
            InstType::JNZ    => write!(f, "jnz"),
            InstType::JMP    => write!(f, "jmp"),
            InstType::LABEL  => write!(f, "label"),
            InstType::BOT    => write!(f, "bot"),
            InstType::DMP    => write!(f, "dmp"),
            InstType::CALL   => write!(f, "call"),
            InstType::RET    => write!(f, "ret"),
            InstType::EXTERN => write!(f, "extern"),
            InstType::HALT   => write!(f, "halt"),
        }
    }
}

impl TryFrom::<&String> for InstType {
    type Error = Trap;

    fn try_from(s: &String) -> Result::<Self, Self::Error> {
        use InstType::*;
        match s.trim() {
            "nop"    => Ok(NOP),
            "push"   => Ok(PUSH),
            "pop"    => Ok(POP),
            "inc"    => Ok(INC),
            "dec"    => Ok(DEC),
            "iadd"   => Ok(IADD),
            "isub"   => Ok(ISUB),
            "imul"   => Ok(IMUL),
            "idiv"   => Ok(IDIV),
            "fadd"   => Ok(FADD),
            "fsub"   => Ok(FSUB),
            "fmul"   => Ok(FMUL),
            "fdiv"   => Ok(FDIV),
            "cmp"    => Ok(CMP),
            "swap"   => Ok(SWAP),
            "dup"    => Ok(DUP),
            "je"     => Ok(JE),
            "jl"     => Ok(JL),
            "jnge"   => Ok(JNGE),
            "jg"     => Ok(JG),
            "jnle"   => Ok(JNLE),
            "jne"    => Ok(JNE),
            "jz"     => Ok(JZ),
            "jnz"    => Ok(JNZ),
            "jmp"    => Ok(JMP),
            "label"  => Ok(LABEL),
            "bot"    => Ok(BOT),
            "dmp"    => Ok(DMP),
            "call"   => Ok(CALL),
            "ret"    => Ok(RET),
            "extern" => Ok(EXTERN),
            "halt"   => Ok(HALT),
            _        => Err(Trap::UndefinedSymbol(s.to_string()))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InstValue {
    U8(u8),
    NaN(NaNBox),
    F64(f64),
    I64(i64),
    U64(u64),
    String(String),
    StringU64(String, u64),
    None
}

macro_rules! match_shit {
    ($s: expr, $f: expr, $($v: tt), *) => {
        match $s {
            $(Self::$v(v) => write!($f, "{v}"),)*
            _ => Ok(())
        }
    };
}

impl std::fmt::Display for InstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Self::StringU64(s, v) = self {
            write!(f, "{s}, {v}")
        } else {
            match_shit!(self, f, U8, NaN, F64, I64, U64, String)
        }
    }
}

macro_rules! _decl_ {
    (g.$name: tt, $ty: ty, $vty: tt) => {
        #[inline]
        pub fn $name(&self) -> Option::<&$ty> {
            match self {
                InstValue::$vty(ret) => Some(ret),
                _ => None
            }
        }
    };
    (a.$name: tt, $fn: tt, $ty: ty) => {
        #[inline]
        #[track_caller]
        pub fn $name(&self) -> $ty {
            self.$fn().unwrap()
        }
    };
}

impl InstValue {
    _decl_!{g.get_u8, u8, U8}
    _decl_!{g.get_nan, NaNBox, NaN}
    _decl_!{g.get_f64, f64, F64}
    _decl_!{g.get_i64, i64, I64}
    _decl_!{g.get_u64, u64, U64}
    _decl_!{g.get_string, String, String}
    _decl_!{a.as_u8, get_u8, &u8}
    _decl_!{a.as_nan, get_nan, &NaNBox}
    _decl_!{a.as_f64, get_f64, &f64}
    _decl_!{a.as_i64, get_i64, &i64}
    _decl_!{a.as_u64, get_u64, &u64}
    _decl_!{a.as_string, get_string, &String}
    _decl_!{a.as_string_u64, get_string_u64, (&String, u64)}

    #[inline]
    pub fn get_string_u64(&self) -> Option::<(&String, u64)> {
        match self {
            InstValue::StringU64(s, u) => Some((s, *u)),
            _ => None
        }
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

macro_rules! _decl_const_ {
    ($v: tt) => {
        pub const $v:  Self = Self { typ: InstType::$v,  val: InstValue::None };
    };
}

impl Inst {
    _decl_const_!{NOP}
    _decl_const_!{POP}
    _decl_const_!{INC}
    _decl_const_!{DEC}
    _decl_const_!{IADD}
    _decl_const_!{ISUB}
    _decl_const_!{IMUL}
    _decl_const_!{IDIV}
    _decl_const_!{FADD}
    _decl_const_!{FSUB}
    _decl_const_!{FMUL}
    _decl_const_!{FDIV}
    _decl_const_!{SWAP}
    _decl_const_!{BOT}
    _decl_const_!{RET}
    _decl_const_!{HALT}
    pub const SIZE: usize = 8;
}

const _: () = assert!(std::mem::size_of::<f64>() == Inst::SIZE, "Mm's designed to be working on 64bit");
const INSTRUCTION_PART: usize = Inst::SIZE + 1;

fn extend_from_bytes_u64(n: u8, val: u64) -> Vec::<u8> {
    let mut bytes = Vec::with_capacity(INSTRUCTION_PART);
    bytes.push(n);
    let le_bytes = val.to_le_bytes();
    bytes.extend(le_bytes);
    bytes
}

fn extend_from_bytes_nan(n: u8, val: &NaNBox) -> Vec::<u8> {
    let mut bytes = Vec::with_capacity(INSTRUCTION_PART);
    bytes.push(n);
    let le_bytes = val.as_f64().to_le_bytes();
    bytes.extend(le_bytes);
    bytes
}

fn extend_to_bytes_string(n: u8, val: &str) -> Vec::<u8> {
    let mut bytes = Vec::with_capacity(128);
    bytes.push(n);
    let str_bytes = val.as_bytes();
    let str_len = str_bytes.len();
    let le_bytes = (str_len as u64).to_le_bytes();
    bytes.extend(le_bytes);
    bytes.extend(str_bytes);
    bytes
}

fn nan_from_bytes(bytes: &[u8]) -> NaNBox {
    let mut array = [0; 8];
    array.copy_from_slice(&bytes[1..Inst::SIZE + 1]);
    let f = f64::from_le_bytes(array);
    NaNBox(f)
}

fn u64_from_bytes(bytes: &[u8]) -> u64 {
    let mut array = [0; 8];
    array.copy_from_slice(&bytes[1..Inst::SIZE + 1]);
    u64::from_le_bytes(array)
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
                val: InstValue::NaN(a)
            };
            Ok((inst, 9))
        } else {
            Err(Trap::InvalidOperand(stringify!($ret).to_owned()))
        }
    }};
    (n.$b: ident, $ret: tt) => {{
        if $b.len() >= 9 {
            let a = u64_from_bytes(&$b);
            let inst = Inst {
                typ: InstType::$ret,
                val: InstValue::U64(a)
            };
            Ok((inst, 9))
        } else {
            Err(Trap::InvalidOperand(stringify!($ret).to_owned()))
        }
    }};
    (u8.$b: ident, $ret: tt) => {{
        if $b.len() >= 2 {
            let inst = Inst {
                typ: InstType::$ret,
                val: InstValue::U8($b[0])
            };
            Ok((inst, 2))
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
    pub fn as_bytes(&self) -> Vec::<u8> {
        match self.typ {
            InstType::NOP    => vec![0],
            InstType::PUSH   => extend_from_bytes_nan(1, &self.val.as_nan()),
            InstType::POP    => vec![2],
            InstType::INC    => vec![3],
            InstType::DEC    => vec![4],
            InstType::IADD   => vec![5],
            InstType::ISUB   => vec![6],
            InstType::IMUL   => vec![7],
            InstType::IDIV   => vec![8],
            InstType::CMP    => extend_from_bytes_nan(9, &self.val.as_nan()),
            InstType::SWAP   => vec![10],
            InstType::DUP    => extend_from_bytes_u64(11, *self.val.as_u64()),
            InstType::JE     => extend_to_bytes_string(12, self.val.as_string()),
            InstType::JL     => extend_to_bytes_string(13, self.val.as_string()),
            InstType::JNGE   => extend_to_bytes_string(14, self.val.as_string()),
            InstType::JG     => extend_to_bytes_string(15, self.val.as_string()),
            InstType::JNLE   => extend_to_bytes_string(16, self.val.as_string()),
            InstType::JNE    => extend_to_bytes_string(17, self.val.as_string()),
            InstType::JZ     => extend_to_bytes_string(18, self.val.as_string()),
            InstType::JNZ    => extend_to_bytes_string(19, self.val.as_string()),
            InstType::JMP    => extend_to_bytes_string(20, self.val.as_string()),
            InstType::LABEL  => extend_to_bytes_string(21, self.val.as_string()),
            InstType::BOT    => vec![22],
            InstType::FADD   => vec![23],
            InstType::FSUB   => vec![24],
            InstType::FMUL   => vec![25],
            InstType::FDIV   => vec![26],
            InstType::DMP    => vec![27, *self.val.as_u8()],
            InstType::CALL   => extend_to_bytes_string(28, self.val.as_string()),
            InstType::RET    => vec![29],
            InstType::EXTERN => vec![30],
            InstType::HALT   => vec![69],
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Inst, usize), Trap> {
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
            Some(11) => inst_from_bytes!(n.bytes, DUP),
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
            Some(27) => inst_from_bytes!(u8.bytes, DMP),
            Some(28) => inst_from_bytes!(bytes, CALL),
            Some(29) => Ok((Inst::RET, 1)),
            Some(69) => Ok((Inst::HALT, 1)),
            _        => Err(Trap::UndefinedSymbol(format!("bytes: {bytes:?}"))),
        }
    }
}

impl From<&Inst> for String {
    fn from(inst: &Inst) -> Self {
        if inst.typ.is_arg_required() {
            match inst.typ {
                InstType::PUSH   => format!("    push    {oper}", oper = inst.val.as_nan()),
                InstType::CMP    => format!("    cmp     {oper}", oper = inst.val.as_nan()),
                InstType::DUP    => format!("    dup     {oper}", oper = inst.val.as_u64()),
                InstType::JE     => format!("    je      {oper}", oper = inst.val.as_string()),
                InstType::JL     => format!("    jl      {oper}", oper = inst.val.as_string()),
                InstType::JNGE   => format!("    jnge    {oper}", oper = inst.val.as_string()),
                InstType::JG     => format!("    jg      {oper}", oper = inst.val.as_string()),
                InstType::JNLE   => format!("    jnle    {oper}", oper = inst.val.as_string()),
                InstType::JNE    => format!("    jne     {oper}", oper = inst.val.as_string()),
                InstType::JZ     => format!("    jz      {oper}", oper = inst.val.as_string()),
                InstType::JNZ    => format!("    jnz     {oper}", oper = inst.val.as_string()),
                InstType::JMP    => format!("    jmp     {oper}", oper = inst.val.as_string()),
                InstType::LABEL  => format!("{oper}:", oper = inst.val.as_string()),
                InstType::CALL   => format!("    call    {oper}", oper = inst.val.as_string()),
                InstType::DMP    => format!("    dmp     {oper}", oper = inst.val.as_u8()),
                InstType::EXTERN => format!("    dmp     {oper}", oper = inst.val.as_string()),
                _ => unreachable!()
            }
        } else {
            format!("    {t}", t = inst.typ)
        }
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let oper = &self.val;
        match self.typ {
            InstType::NOP    => write!(f, "Instruction: `NOP`"),
            InstType::PUSH   => { write!(f, "Instruction: `PUSH`, operand: `{oper}`") }
            InstType::POP    => write!(f, "Instruction: `POP`"),
            InstType::INC    => write!(f, "Instruction: `INC`"),
            InstType::DEC    => write!(f, "Instruction: `DEC`"),
            InstType::IADD   => write!(f, "Instruction: `IADD`"),
            InstType::ISUB   => write!(f, "Instruction: `ISUB`"),
            InstType::IMUL   => write!(f, "Instruction: `IMUL`"),
            InstType::IDIV   => write!(f, "Instruction: `IDIV`"),
            InstType::FADD   => write!(f, "Instruction: `FADD`"),
            InstType::FSUB   => write!(f, "Instruction: `FSUB`"),
            InstType::FMUL   => write!(f, "Instruction: `FMUL`"),
            InstType::FDIV   => write!(f, "Instruction: `FDIV`"),
            InstType::CMP    => { write!(f, "Instruction: `CMP`, operand: `{oper}`") }
            InstType::SWAP   => write!(f, "Instruction: `SWAP`"),
            InstType::DUP    => { write!(f, "Instruction: `DUP`, operand: `{oper}`", oper =  self.val.as_u64()) }
            InstType::JE     => write!(f, "Instruction: `JE`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JL     => write!(f, "Instruction: `JL`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JNGE   => write!(f, "Instruction: `JNGE`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JG     => write!(f, "Instruction: `JG`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JNLE   => write!(f, "Instruction: `JNLE`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JNE    => write!(f, "Instruction: `JNE`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JZ     => write!(f, "Instruction: `JZ`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JNZ    => write!(f, "Instruction: `JNZ`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JMP    => write!(f, "Instruction: `JMP`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::LABEL  => write!(f, "Instruction: `LABEL`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::CALL   => write!(f, "Instruction: `CALL`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::BOT    => write!(f, "Instruction: `BOT`"),
            InstType::RET    => write!(f, "Instruction: `RET`"),
            InstType::DMP    => write!(f, "Instruction: `DMP`, operand: {oper}", oper = self.val.as_u8()),
            InstType::EXTERN => write!(f, "Instruction: `DMP`, operand: {oper}", oper = self.val.as_string()),
            InstType::HALT   => write!(f, "Instruction: `HALT`"),
        }
    }
}

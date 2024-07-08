use std::fmt::Display;

use crate::{NaNBox, Trap};

#[derive(Eq, Hash, Clone, PartialEq)]
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
    JLE,
    JG,
    JGE,
    JNE,
    JZ,
    JNZ,
    JMP,
    LABEL,

    F2I,
    F2U,
    I2F,
    I2U,
    U2I,
    U2F,

    BOT,
    DMP,
    CALL,
    RET,
    EXTERN,
    NATIVE,

    READ8,
    READ16,
    READ32,
    READ64,

    WRITE8,
    WRITE16,
    WRITE32,
    WRITE64,

    HALT,
}

impl InstType {
    pub fn try_from_string<'a>(s: &'a String) -> Result::<Self, Trap> {
        match s.trim() {
            "nop"     => Ok(InstType::NOP),
            "push"    => Ok(InstType::PUSH),
            "pop"     => Ok(InstType::POP),
            "inc"     => Ok(InstType::INC),
            "dec"     => Ok(InstType::DEC),
            "iadd"    => Ok(InstType::IADD),
            "isub"    => Ok(InstType::ISUB),
            "imul"    => Ok(InstType::IMUL),
            "idiv"    => Ok(InstType::IDIV),
            "fadd"    => Ok(InstType::FADD),
            "fsub"    => Ok(InstType::FSUB),
            "fmul"    => Ok(InstType::FMUL),
            "fdiv"    => Ok(InstType::FDIV),
            "cmp"     => Ok(InstType::CMP),
            "swap"    => Ok(InstType::SWAP),
            "dup"     => Ok(InstType::DUP),
            "je"      => Ok(InstType::JE),
            "jl"      => Ok(InstType::JL),
            "jnge"    => Ok(InstType::JLE),
            "jle"     => Ok(InstType::JLE),
            "jg"      => Ok(InstType::JG),
            "jnle"    => Ok(InstType::JGE),
            "jge"     => Ok(InstType::JGE),
            "jne"     => Ok(InstType::JNE),
            "jz"      => Ok(InstType::JZ),
            "jnz"     => Ok(InstType::JNZ),
            "jmp"     => Ok(InstType::JMP),
            "label"   => Ok(InstType::LABEL),
            "bot"     => Ok(InstType::BOT),
            "dmp"     => Ok(InstType::DMP),
            "call"    => Ok(InstType::CALL),
            "ret"     => Ok(InstType::RET),
            "extern"  => Ok(InstType::EXTERN),
            "native"  => Ok(InstType::NATIVE),
            "f2i"     => Ok(InstType::F2I),
            "f2u"     => Ok(InstType::F2U),
            "i2f"     => Ok(InstType::I2F),
            "i2u"     => Ok(InstType::I2U),
            "u2i"     => Ok(InstType::U2I),
            "u2f"     => Ok(InstType::U2F),
            "read8"   => Ok(InstType::READ8),
            "write8"  => Ok(InstType::WRITE8),
            "read16"  => Ok(InstType::READ16),
            "write16" => Ok(InstType::WRITE16),
            "read32"  => Ok(InstType::READ32),
            "write32" => Ok(InstType::WRITE32),
            "read64"  => Ok(InstType::READ64),
            "write64" => Ok(InstType::WRITE64),
            "halt"    => Ok(InstType::HALT),
            _         => Err(Trap::UndefinedSymbol(s.to_owned()))
        }
    }

    pub fn is_arg_required(&self) -> bool {
        match self {
              InstType::PUSH
            | InstType::DUP
            | InstType::JE
            | InstType::JL
            | InstType::JLE
            | InstType::JG
            | InstType::JGE
            | InstType::SWAP
            | InstType::JNE
            | InstType::JZ
            | InstType::JNZ
            | InstType::JMP
            | InstType::LABEL
            | InstType::DMP
            | InstType::CALL
            | InstType::NATIVE
            | InstType::EXTERN => true,
            _ => false
        }
    }
}

impl std::fmt::Display for InstType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstType::NOP     => write!(f, "nop"),
            InstType::PUSH    => write!(f, "push"),
            InstType::POP     => write!(f, "pop"),
            InstType::INC     => write!(f, "inc"),
            InstType::DEC     => write!(f, "dec"),
            InstType::IADD    => write!(f, "iadd"),
            InstType::ISUB    => write!(f, "isub"),
            InstType::IMUL    => write!(f, "imul"),
            InstType::IDIV    => write!(f, "idiv"),
            InstType::FADD    => write!(f, "fadd"),
            InstType::FSUB    => write!(f, "fsub"),
            InstType::FMUL    => write!(f, "fmul"),
            InstType::FDIV    => write!(f, "fdiv"),
            InstType::CMP     => write!(f, "cmp"),
            InstType::SWAP    => write!(f, "swap"),
            InstType::DUP     => write!(f, "dup"),
            InstType::JE      => write!(f, "je"),
            InstType::JL      => write!(f, "jl"),
            InstType::JLE     => write!(f, "jnge"),
            InstType::JG      => write!(f, "jg"),
            InstType::JGE     => write!(f, "jnle"),
            InstType::JNE     => write!(f, "jne"),
            InstType::JZ      => write!(f, "jz"),
            InstType::JNZ     => write!(f, "jnz"),
            InstType::JMP     => write!(f, "jmp"),
            InstType::LABEL   => write!(f, "label"),
            InstType::F2I     => write!(f, "f2i"),
            InstType::F2U     => write!(f, "f2u"),
            InstType::I2F     => write!(f, "i2f"),
            InstType::I2U     => write!(f, "i2u"),
            InstType::U2I     => write!(f, "u2i"),
            InstType::U2F     => write!(f, "u2f"),
            InstType::BOT     => write!(f, "bot"),
            InstType::DMP     => write!(f, "dmp"),
            InstType::CALL    => write!(f, "call"),
            InstType::RET     => write!(f, "ret"),
            InstType::EXTERN  => write!(f, "extern"),
            InstType::NATIVE  => write!(f, "native"),
            InstType::READ8   => write!(f, "read8"),
            InstType::WRITE8  => write!(f, "write8"),
            InstType::READ16  => write!(f, "read16"),
            InstType::WRITE16 => write!(f, "write16"),
            InstType::READ32  => write!(f, "read32"),
            InstType::WRITE32 => write!(f, "write32"),
            InstType::READ64  => write!(f, "read64"),
            InstType::WRITE64 => write!(f, "write64"),
            InstType::HALT    => write!(f, "halt"),
        }
    }
}

#[derive(Clone)]
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

macro_rules! _display_match_shit {
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
            _display_match_shit!(self, f, U8, NaN, F64, I64, U64, String)
        }
    }
}

macro_rules! _decl_ {
    (g.$name: tt, $ty: ty, $vty: tt) => {
        #[inline(always)]
        pub fn $name(&self) -> Option::<&$ty> {
            match self {
                InstValue::$vty(ret) => Some(ret),
                _ => None
            }
        }
    };
    (a.$name: tt, $fn: tt, $ty: ty) => {
        #[inline(always)]
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

    #[inline(always)]
    pub fn get_string_u64(&self) -> Option::<(&String, u64)> {
        match self {
            InstValue::StringU64(s, u) => Some((s, *u)),
            _ => None
        }
    }
}

#[derive(Clone)]
pub struct Inst {
    pub typ: InstType,
    pub val: InstValue
}

impl std::fmt::Debug for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.typ.fmt(f)?;
        self.val.fmt(f)
    }
}

impl PartialEq for Inst {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

macro_rules! _match_typeshit {
    ($t_: expr, $($t: tt), *) => {
        match $t_ {
            $(InstType::$t => Ok(Self::$t),)*
            _ => Err(())
        }
    };
}

impl TryFrom::<InstType> for Inst {
    type Error = ();

    fn try_from(typ: InstType) -> Result<Self, Self::Error> {
        _match_typeshit! {typ, NOP, POP, INC, CMP, DEC, IADD, ISUB, IMUL, IDIV, FADD, FSUB, FMUL, FDIV, BOT, RET, F2I, F2U, I2F, I2U, U2I, U2F, READ8, READ16, READ32, READ64, WRITE8, WRITE16, WRITE32, WRITE64, HALT}
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
    _decl_const_!{CMP}
    _decl_const_!{IADD}
    _decl_const_!{ISUB}
    _decl_const_!{IMUL}
    _decl_const_!{IDIV}
    _decl_const_!{FADD}
    _decl_const_!{FSUB}
    _decl_const_!{FMUL}
    _decl_const_!{FDIV}
    _decl_const_!{BOT}
    _decl_const_!{F2I}
    _decl_const_!{F2U}
    _decl_const_!{I2F}
    _decl_const_!{I2U}
    _decl_const_!{U2I}
    _decl_const_!{U2F}
    _decl_const_!{RET}
    _decl_const_!{HALT}
    _decl_const_!{READ8}
    _decl_const_!{READ16}
    _decl_const_!{READ32}
    _decl_const_!{READ64}
    _decl_const_!{WRITE8}
    _decl_const_!{WRITE16}
    _decl_const_!{WRITE32}
    _decl_const_!{WRITE64}
    pub const SIZE: usize = 8;
}

const _: () = assert!(std::mem::size_of::<f64>() == Inst::SIZE, "Mm's designed to be working on 64bit");
const INSTRUCTION_PART: usize = Inst::SIZE + 1;

#[inline]
fn extend_from_bytes_u64(n: u8, val: u64) -> Vec::<u8> {
    let mut bytes = Vec::with_capacity(INSTRUCTION_PART);
    bytes.push(n);
    let le_bytes = val.to_le_bytes();
    bytes.extend(le_bytes);
    bytes
}

#[inline]
fn extend_from_bytes_nan(n: u8, val: &NaNBox) -> Vec::<u8> {
    let mut bytes = Vec::with_capacity(INSTRUCTION_PART);
    bytes.push(n);
    let le_bytes = val.as_f64().to_le_bytes();
    bytes.extend(le_bytes);
    bytes
}

#[inline]
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

#[inline]
fn nan_from_bytes(bytes: &[u8]) -> NaNBox {
    let mut array = [0; 8];
    array.copy_from_slice(&bytes[1..Inst::SIZE + 1]);
    let f = f64::from_le_bytes(array);
    NaNBox(f)
}

#[inline]
fn u64_from_bytes(bytes: &[u8]) -> u64 {
    let mut array = [0; 8];
    array.copy_from_slice(&bytes[1..Inst::SIZE + 1]);
    u64::from_le_bytes(array)
}

#[inline]
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
            Err(Trap::InvalidOperand(stringify!($ret)))
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
            Err(Trap::InvalidOperand(stringify!($ret)))
        }
    }};
    (u8.$b: ident, $ret: tt) => {{
        if $b.len() >= 2 {
            let inst = Inst {
                typ: InstType::$ret,
                val: InstValue::U8($b[1])
            };
            Ok((inst, 2))
        } else {
            Err(Trap::InvalidOperand(stringify!($ret)))
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
                Err(Trap::InvalidOperand(stringify!($ret)))
            }
        } else {
            Err(Trap::InvalidOperand(stringify!($ret)))
        }
    }}
}

impl Inst {
    pub fn as_bytes(&self) -> Vec::<u8> {
        match self.typ {
            InstType::NOP     => vec![0],
            InstType::PUSH    => extend_from_bytes_nan(1, &self.val.as_nan()),
            InstType::POP     => vec![2],
            InstType::INC     => vec![3],
            InstType::DEC     => vec![4],
            InstType::IADD    => vec![5],
            InstType::ISUB    => vec![6],
            InstType::IMUL    => vec![7],
            InstType::IDIV    => vec![8],
            InstType::CMP     => extend_from_bytes_nan(9, &self.val.as_nan()),
            InstType::SWAP    => extend_from_bytes_u64(10, *self.val.as_u64()),
            InstType::DUP     => extend_from_bytes_u64(11, *self.val.as_u64()),
            InstType::JE      => extend_to_bytes_string(12, self.val.as_string()),
            InstType::JL      => extend_to_bytes_string(13, self.val.as_string()),
            InstType::JLE     => extend_to_bytes_string(14, self.val.as_string()),
            InstType::JG      => extend_to_bytes_string(15, self.val.as_string()),
            InstType::JGE     => extend_to_bytes_string(16, self.val.as_string()),
            InstType::JNE     => extend_to_bytes_string(17, self.val.as_string()),
            InstType::JZ      => extend_to_bytes_string(18, self.val.as_string()),
            InstType::JNZ     => extend_to_bytes_string(19, self.val.as_string()),
            InstType::JMP     => extend_to_bytes_string(20, self.val.as_string()),
            InstType::LABEL   => extend_to_bytes_string(21, self.val.as_string()),
            InstType::BOT     => vec![22],
            InstType::FADD    => vec![23],
            InstType::FSUB    => vec![24],
            InstType::FMUL    => vec![25],
            InstType::FDIV    => vec![26],
            InstType::DMP     => vec![27, *self.val.as_u8()],
            InstType::CALL    => extend_to_bytes_string(28, self.val.as_string()),
            InstType::RET     => vec![29],
            InstType::EXTERN  => vec![30],
            InstType::F2I     => vec![31],
            InstType::F2U     => vec![32],
            InstType::I2F     => vec![33],
            InstType::I2U     => vec![34],
            InstType::U2I     => vec![35],
            InstType::U2F     => vec![36],
            InstType::NATIVE  => vec![37],
            InstType::READ8   => extend_from_bytes_u64(38, *self.val.as_u64()),
            InstType::WRITE8  => extend_from_bytes_u64(39, *self.val.as_u64()),
            InstType::READ16  => extend_from_bytes_u64(40, *self.val.as_u64()),
            InstType::WRITE16 => extend_from_bytes_u64(41, *self.val.as_u64()),
            InstType::READ32  => extend_from_bytes_u64(42, *self.val.as_u64()),
            InstType::WRITE32 => extend_from_bytes_u64(43, *self.val.as_u64()),
            InstType::READ64  => extend_from_bytes_u64(44, *self.val.as_u64()),
            InstType::WRITE64 => extend_from_bytes_u64(45, *self.val.as_u64()),
            InstType::HALT    => vec![69],
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Result::<(Inst, usize), Trap> {
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
            Some(10) => inst_from_bytes!(n.bytes, SWAP),
            Some(11) => inst_from_bytes!(n.bytes, DUP),
            Some(12) => inst_from_bytes!(bytes, JE),
            Some(13) => inst_from_bytes!(bytes, JL),
            Some(14) => inst_from_bytes!(bytes, JLE),
            Some(15) => inst_from_bytes!(bytes, JG),
            Some(16) => inst_from_bytes!(bytes, JGE),
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
            Some(30) => inst_from_bytes!(bytes, EXTERN),
            Some(31) => Ok((Inst::F2I, 1)),
            Some(32) => Ok((Inst::F2U, 1)),
            Some(33) => Ok((Inst::I2F, 1)),
            Some(34) => Ok((Inst::I2U, 1)),
            Some(35) => Ok((Inst::U2I, 1)),
            Some(36) => Ok((Inst::U2F, 1)),
            Some(37) => inst_from_bytes!(bytes, NATIVE),
            Some(38) => inst_from_bytes!(n.bytes, READ8),
            Some(39) => inst_from_bytes!(n.bytes, WRITE8),
            Some(40) => inst_from_bytes!(n.bytes, READ16),
            Some(41) => inst_from_bytes!(n.bytes, WRITE16),
            Some(42) => inst_from_bytes!(n.bytes, READ32),
            Some(43) => inst_from_bytes!(n.bytes, WRITE32),
            Some(44) => inst_from_bytes!(n.bytes, READ64),
            Some(45) => inst_from_bytes!(n.bytes, WRITE64),
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
                InstType::DUP    => format!("    dup     {oper}", oper = inst.val.as_u64()),
                InstType::JE     => format!("    je      {oper}", oper = inst.val.as_string()),
                InstType::JL     => format!("    jl      {oper}", oper = inst.val.as_string()),
                InstType::JLE    => format!("    jnge    {oper}", oper = inst.val.as_string()),
                InstType::JG     => format!("    jg      {oper}", oper = inst.val.as_string()),
                InstType::JGE    => format!("    jnle    {oper}", oper = inst.val.as_string()),
                InstType::JNE    => format!("    jne     {oper}", oper = inst.val.as_string()),
                InstType::JZ     => format!("    jz      {oper}", oper = inst.val.as_string()),
                InstType::JNZ    => format!("    jnz     {oper}", oper = inst.val.as_string()),
                InstType::JMP    => format!("    jmp     {oper}", oper = inst.val.as_string()),
                InstType::LABEL  => format!("{oper}:",            oper = inst.val.as_string()),
                InstType::CALL   => format!("    call    {oper}", oper = inst.val.as_string()),
                InstType::DMP    => format!("    dmp     {oper}", oper = inst.val.as_u8()),
                InstType::SWAP   => format!("    swap    {oper}", oper = inst.val.as_u64()),
                InstType::EXTERN => format!("    extern  {oper}", oper = inst.val.as_string()),
                InstType::NATIVE => format!("    native  {oper}", oper = inst.val.as_string()),
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
            InstType::NOP     => write!(f, "`NOP`"),
            InstType::PUSH    => { write!(f, "`PUSH`, operand: `{oper}`") }
            InstType::POP     => write!(f, "`POP`"),
            InstType::INC     => write!(f, "`INC`"),
            InstType::DEC     => write!(f, "`DEC`"),
            InstType::IADD    => write!(f, "`IADD`"),
            InstType::ISUB    => write!(f, "`ISUB`"),
            InstType::IMUL    => write!(f, "`IMUL`"),
            InstType::IDIV    => write!(f, "`IDIV`"),
            InstType::FADD    => write!(f, "`FADD`"),
            InstType::FSUB    => write!(f, "`FSUB`"),
            InstType::FMUL    => write!(f, "`FMUL`"),
            InstType::FDIV    => write!(f, "`FDIV`"),
            InstType::CMP     => { write!(f, "`CMP`") }
            InstType::SWAP    => write!(f, "`SWAP`, operand: `{oper}`", oper =  self.val.as_u64()),
            InstType::DUP     => { write!(f, "`DUP`, operand: `{oper}`", oper =  self.val.as_u64()) }
            InstType::JE      => write!(f, "`JE`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JL      => write!(f, "`JL`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JLE     => write!(f, "`JLE`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JG      => write!(f, "`JG`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JGE     => write!(f, "`JGE`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JNE     => write!(f, "`JNE`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JZ      => write!(f, "`JZ`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JNZ     => write!(f, "`JNZ`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::JMP     => write!(f, "`JMP`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::LABEL   => write!(f, "`LABEL`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::CALL    => write!(f, "`CALL`, operand: `{oper}`", oper = self.val.as_string()),
            InstType::F2I     => write!(f, "`F2I`"),
            InstType::F2U     => write!(f, "`F2U`"),
            InstType::I2F     => write!(f, "`I2F`"),
            InstType::I2U     => write!(f, "`I2U`"),
            InstType::U2I     => write!(f, "`U2I`"),
            InstType::U2F     => write!(f, "`U2F`"),
            InstType::BOT     => write!(f, "`BOT`"),
            InstType::RET     => write!(f, "`RET`"),
            InstType::DMP     => write!(f, "`DMP`, operand: {oper}", oper = self.val.as_u8()),
            InstType::EXTERN  => write!(f, "`EXTERN`, operand: {oper}", oper = self.val.as_string()),
            InstType::NATIVE  => write!(f, "`NATIVE`, operand: {oper}", oper = self.val.as_string()),
            InstType::READ8   => write!(f, "`READ8`"),
            InstType::WRITE8  => write!(f, "`WRITE8`"),
            InstType::READ16  => write!(f, "`READ16`"),
            InstType::WRITE16 => write!(f, "`WRITE16`"),
            InstType::READ32  => write!(f, "`READ32`"),
            InstType::WRITE32 => write!(f, "`WRITE32`"),
            InstType::READ64  => write!(f, "`READ64`"),
            InstType::WRITE64 => write!(f, "`WRITE64`"),
            InstType::HALT    => write!(f, "`HALT`"),
        }
    }
}

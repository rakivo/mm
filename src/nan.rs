use std::mem::transmute;

type Ptr = *const u8;

#[repr(u64)]
#[derive(Debug, PartialEq)]
pub enum Type {
    Double = 0,
    Int,
    Ptr
}

impl From::<f64> for Type {
    fn from(value: f64) -> Self {
        if value.is_nan() {
            Type::Int
        } else {
            Type::Double
        }
    }
}

impl From::<Word> for Type {
    fn from(_: Word) -> Self {
        Type::Int
    }
}

impl From::<Ptr> for Type {
    fn from(_: Ptr) -> Self {
        Type::Ptr
    }
}

pub trait Boxable {
    fn get_type(&self) -> Type;
    fn get_value(&self) -> Word;

    fn set_type(&mut self, typef: Type) -> &mut Self;
    fn set_value(&mut self, value: Word) -> &mut Self;

    fn is_f64(&self) -> bool;
    fn is_word(&self) -> bool;
    fn is_ptr(&self) -> bool;

    fn as_f64(&self) -> f64;
    fn as_word(&self) -> Word;
    fn nan_as_ptr(&self) -> Ptr;

    fn from_f64(value: f64) -> Self;
    fn from_word(value: Word) -> Self;
    fn from_ptr(value: Ptr) -> Self;
}

const TYPE_BITS: Word = 8;
const PAYLOAD_BITS: Word = 54;

impl Boxable for f64 {
    #[inline(always)]
    fn get_type(&self) -> Type {
        let bits: Word = unsafe { transmute(*self) };
        unsafe { transmute((bits >> PAYLOAD_BITS) & ((1 << TYPE_BITS) - 1)) }
    }

    #[inline(always)]
    fn get_value(&self) -> Word {
        let bits: Word = unsafe { transmute(*self) };
        bits & ((1 << PAYLOAD_BITS) - 1)
    }

    #[inline(always)]
    fn set_type(&mut self, typef: Type) -> &mut Self {
        let bits: Word = unsafe { transmute(*self) };
        let new_bits = (bits & !((1 << TYPE_BITS) - 1 << PAYLOAD_BITS)) | (((typef as Word) & ((1 << TYPE_BITS) - 1)) << PAYLOAD_BITS);
        *self = unsafe { transmute(new_bits) };
        self
    }

    #[inline(always)]
    fn set_value(&mut self, value: Word) -> &mut Self {
        let bits: Word = unsafe { transmute(*self) };
        let new_bits = (bits & !((1 << PAYLOAD_BITS) - 1)) | (value & ((1 << PAYLOAD_BITS) - 1));
        *self = unsafe { transmute(new_bits) };
        self
    }

    #[inline(always)]
    fn is_f64(&self) -> bool {
        !self.is_nan()
    }

    #[inline(always)]
    fn is_word(&self) -> bool {
        self.is_nan()
    }

    #[inline(always)]
    fn is_ptr(&self) -> bool {
        self.get_type() == Type::Ptr
    }

    #[inline(always)]
    fn as_f64(&self) -> f64 {
        *self
    }

    #[inline(always)]
    fn as_word(&self) -> Word {
        self.get_value()
    }

    #[inline(always)]
    fn nan_as_ptr(&self) -> Ptr {
        self.get_value() as _
    }

    #[inline(always)]
    fn from_f64(value: f64) -> Self {
        value
    }

    #[inline(always)]
    fn from_word(value: Word) -> Self {
        *(1.0_f64 / 0.0_f64).set_type(Type::Int).set_value(value)
    }

    #[inline(always)]
    fn from_ptr(value: Ptr) -> Self {
        *(1.0_f64 / 0.0_f64).set_type(Type::Ptr).set_value(value as _)
    }
}

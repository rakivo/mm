use crate::InstType;

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum Flag {E, L, LE, G, GE, NE, Z, NZ}

impl TryFrom::<&InstType> for Flag {
    type Error = ();

    fn try_from(inst: &InstType) -> Result<Self, Self::Error> {
        use { InstType::*, Flag::* };
        match inst {
            JE  => Ok(E),
            JL  => Ok(L),
            JLE => Ok(LE),
            JG  => Ok(G),
            JGE => Ok(GE),
            JNE => Ok(NE),
            JZ  => Ok(Z),
            JNZ => Ok(NZ),
            _   => Err(())
        }
    }
}

pub struct Flags(u8);

impl std::fmt::Display for Flags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#08}", self.0)
    }
}

macro_rules! ifc {
    ($s: expr, $c: expr, $($f1: tt), * | $($f2: tt), *) => {
        if $c {
            $($s.set(Flag::$f1);)*
            $($s.reset(Flag::$f2);)*
        } else {
            $($s.set(Flag::$f2);)*
            $($s.reset(Flag::$f1);)*
        }
    };
}

impl Flags {
    #[inline(always)]
    pub fn new() -> Flags {
        Flags(0)
    }

    pub fn cmp(&mut self, a: u64, b: u64) {
        self.reset_all();
        ifc!(self, a == b, E, Z           | NE, LE, GE, NZ);
        ifc!(self, a < b,  L, LE          | G, GE);
        ifc!(self, a != 0, NZ             | Z);
    }

    #[inline(always)]
    pub fn set(&mut self, flag: Flag) {
        self.0 |= 1 << flag as u8
    }

    #[inline(always)]
    pub fn reset_all(&mut self) {
        self.0 = 0;
    }

    #[inline(always)]
    pub fn reset(&mut self, flag: Flag) {
        self.0 &= !(1 << flag as u8)
    }

    #[inline(always)]
    pub fn is(&self, flag: Flag) -> bool {
        (self.0 & (1 << flag as u8)) >> flag as u8 == 1
    }
}

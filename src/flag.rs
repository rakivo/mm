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
            _ => Err(())
        }
    }
}

pub struct Flags(u8);

impl Flags {
    #[inline(always)]
    pub fn new() -> Flags {
        Flags(0)
    }

    #[inline(always)]
    fn if_c(&mut self, flag: Flag, c: bool) {
        if c {
            self.set(flag);
        } else {
            self.reset(flag);
        }
    }

    pub fn cmp(&mut self, a: u64, b: u64) {
        self.if_c(Flag::E,   a == b);
        self.if_c(Flag::NE,  a != b);
        self.if_c(Flag::L,   a < b);
        self.if_c(Flag::LE,  a < b);
        self.if_c(Flag::G,   a > b);
        self.if_c(Flag::GE,  a > b);
        self.if_c(Flag::Z,   a == 0);
        self.if_c(Flag::NZ,  a != 0);
    }

    #[inline(always)]
    pub fn set(&mut self, flag: Flag) {
        self.0 |= 1 << flag as u8
    }

    #[inline(always)]
    pub fn reset(&mut self, flag: Flag) {
        self.0 &= !(1 << flag as u8)
    }

    #[inline(always)]
    pub fn is(&self, flag: Flag) -> bool {
        (self.0 & (1 << flag as u8)) >> flag as u8 != 0
    }
}

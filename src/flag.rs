use crate::InstType;

#[derive(Debug, PartialEq)]
pub enum Flag {E, L, NGE, G, NLE, NE, Z, NZ}

impl TryFrom::<&InstType> for Flag {
    type Error = ();

    fn try_from(inst: &InstType) -> Result<Self, Self::Error> {
        use { InstType::*, Flag::* };
        match inst {
            JE   => Ok(E),
            JL   => Ok(L),
            JNGE => Ok(NGE),
            JG   => Ok(G),
            JNLE => Ok(NLE),
            JNE  => Ok(NE),
            JZ   => Ok(Z),
            JNZ  => Ok(NZ),
            _ => Err(())
        }
    }
}

pub struct Flags([bool; 8]);

impl Flags {
    #[inline]
    pub fn new() -> Flags {
        Flags([false; 8])
    }

    fn if_c(&mut self, flag: Flag, c: bool) {
        if c {
            self.set(flag);
        } else {
            self.reset(flag);
        }
    }

    pub fn cmp(&mut self, a: &u64, b: &u64) {
        self.if_c(Flag::E, a == b);
        self.if_c(Flag::NE, a != b);
        self.if_c(Flag::L, a < b);
        self.if_c(Flag::NGE, a < b);
        self.if_c(Flag::G, a > b);
        self.if_c(Flag::NLE, a > b);
        self.if_c(Flag::Z, *a == 0);
        self.if_c(Flag::NZ, *a != 0);
    }

    #[inline]
    pub fn set(&mut self, flag: Flag) {
        self.0[flag as usize] = true;
    }

    #[inline]
    pub fn reset(&mut self, flag: Flag) {
        self.0[flag as usize] = false;
    }

    #[inline]
    pub fn is(&self, flag: Flag) -> bool {
        self.0[flag as usize]
    }
}

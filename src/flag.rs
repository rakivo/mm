use crate::Inst;

#[derive(Debug, PartialEq)]
pub enum Flag {E, L, NGE, G, NLE, NE, Z, NZ}

impl TryFrom::<&Inst> for Flag {
    type Error = ();

    fn try_from(inst: &Inst) -> Result<Self, Self::Error> {
        use { Inst::*, Flag::* };
        match inst {
            JE(..)   => Ok(E),
            JL(..)   => Ok(L),
            JNGE(..) => Ok(NGE),
            JG(..)   => Ok(G),
            JNLE(..) => Ok(NLE),
            JNE(..)  => Ok(NE),
            JZ(..)   => Ok(Z),
            JNZ(..)  => Ok(NZ),
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

    pub fn cmp(&mut self, a: &u64, b: &u64) {
        if a == b {
            self.set(Flag::E);
            self.reset(Flag::NE);
        } else {
            self.reset(Flag::E);
            self.set(Flag::NE);
        }
        if a < b {
            self.set(Flag::L);
            self.set(Flag::NGE);
        } else {
            self.reset(Flag::L);
            self.reset(Flag::NGE);
        }
        if a > b {
            self.set(Flag::G);
            self.set(Flag::NLE);
        } else {
            self.reset(Flag::G);
            self.reset(Flag::NLE);
        }
        if *a == 0 {
            self.set(Flag::Z);
        } else {
            self.reset(Flag::Z);
        }
        if *a != 0 {
            self.set(Flag::NZ);
        } else {
            self.reset(Flag::NZ);
        }
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

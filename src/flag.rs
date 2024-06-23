use crate::Word;

#[derive(Debug, PartialEq)]
pub enum Flag {E, L, NGE, G, NLE, NE, Z, NZ}

pub struct Flags([bool; 8]);

impl Flags {
    #[inline]
    pub fn new() -> Flags {
        Flags([false; 8])
    }

    pub fn cmp(&mut self, a: &Word, b: &Word) {
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

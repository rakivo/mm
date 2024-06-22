use crate::Word;

#[derive(Debug, PartialEq)]
pub enum Flag {E, L, NGE, G, NLE, Z, NZ}

pub struct Flags([bool; 7]);

impl Flags {
    #[inline]
    pub fn new() -> Flags {
        Flags([false; 7])
    }

    pub fn cmp(&mut self, a: &Word, b: &Word) {
        if a == b {
            self.set(Flag::E);
        } else {
            self.reset(Flag::E);
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

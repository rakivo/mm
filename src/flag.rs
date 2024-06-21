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
            self.set(Flag::Z);
        } else if a < b {
            self.set(Flag::L);
            self.set(Flag::NZ);
            self.set(Flag::NGE);
        } else {
            self.set(Flag::G);
            self.set(Flag::NZ);
            self.set(Flag::NLE);
        }
    }

    #[inline]
    pub fn set(&mut self, flag: Flag) {
        self.0[flag as usize] = true;
    }

    #[inline]
    pub fn reset(&mut self) {
        self.0.iter_mut().for_each(|i| *i = false);
    }

    #[inline]
    pub fn is(&self, flag: Flag) -> bool {
        self.0[flag as usize]
    }
}

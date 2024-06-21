#[derive(Clone, Debug)]
pub enum Trap {
    StackOverflow,
    StackUnderflow,
    DivisionByZero,
    InvalidOperand(Option::<String>),
    IllegalInstruction(Option::<String>),
    IllegalInstructionAccess
}

impl From<Option<String>> for Trap {
    fn from(value: Option<String>) -> Self {
        Trap::InvalidOperand(value)
    }
}

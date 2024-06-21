#[derive(Debug)]
pub enum Trap {
    StackOverflow,
    StackUnderflow,
    DivisionByZero,
    InvalidOperand,
    IllegalInstruction,
    IllegalInstructionAccess
}

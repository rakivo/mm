use std::env;

extern crate mm;
use mm::*;

use mm::Inst::{self, *};

const PROGRAM: &[Inst] = &[
    PUSH(34),
    PUSH(35),
    ADD
];

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("USAGE: {prog} <output>", prog = args[0]);
        return Ok(())
    }

    let file_path = &args[1];
    Mm::generate_masm(PROGRAM, file_path)?;

    Ok(())
}

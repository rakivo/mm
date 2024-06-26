use std::env;

extern crate mm;
use mm::*;

use mm::Inst::{self, *};

const PROGRAM: &[Inst] = &[
    PUSH(Word {as_u64: 34}),
    PUSH(Word {as_u64: 35}),
    ADD
];

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("USAGE: {prog} <output>", prog = args[0]);
        return Ok(())
    }

    let file_path = &args[1];
    let mm = Mm::new_slice(PROGRAM);
    mm.generate_masm(file_path)?;

    Ok(())
}

use std::env;

extern crate mm;
use mm::*;

use Inst::*;

fn main() -> std::io::Result<()> {
    let program: &[Inst] = &[
        PUSH(Word {as_u64: 34}),
        PUSH(Word {as_u64: 35}),
        ADD,
        JMP("end".to_owned()),
        LABEL("add".to_owned()),
        ADD,
        LABEL("end".to_owned()),
        HALT
    ];

    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("USAGE: {prog} <output>", prog = args[0]);
        return Ok(())
    }

    let file_path = &args[1];
    let mm = Mm::new_slice(program);
    mm.to_binary(file_path)
}

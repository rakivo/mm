use std::env;

extern crate mm;
use mm::*;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let len = args.len();

    if len < 2 {
        eprintln!("USAGE: {prog} <input> [-o <output>]", prog = args[0]);
        return
    }

    let input = &args[1];
    let mut mm = Mm::try_from_masm(&input).unwrap_or_report();

    match len - 1 {
        1 => mm.execute_program(false, None).unwrap_or_report(),
        2 => mm.to_binary(&args[2]).unwrap_or_report(),
        x @ _ => panic!("Invalid amount of arguments, expected: 1 or 2, got: {x}")
    };
}

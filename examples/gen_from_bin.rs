use std::env;

extern crate mm;
use mm::*;

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 3 {
        eprintln!("USAGE: {prog} <input> <output>", prog = args[0]);
        return Ok(())
    }

    let input_path = &args[1];
    let output_path = &args[2];

    let mm = Mm::from_binary(input_path).unwrap_or_report();
    mm.generate_masm(output_path)
}

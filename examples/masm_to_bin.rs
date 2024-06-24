use std::env;

extern crate mm;
use mm::*;

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 3 {
        eprintln!("USAGE: {prog} <input> <output>", prog = args[0]);
        return Ok(())
    }

    let input_file_path = &args[1];
    let output_file_path = &args[2];
    println!("{input_file_path}, {output_file_path}");

    let mm = Mm::try_from_masm(&input_file_path).unwrap_or_report();
    mm.to_binary(&output_file_path)
}

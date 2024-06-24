use std::env;

extern crate mm;
use mm::*;

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("USAGE: {prog} <input>", prog = args[0]);
        return Ok(())
    }

    let input_file_path = &args[1];
    let mut mm = Mm::try_from_masm(&input_file_path).unwrap_or_report();
    while !mm.halt() {
        mm.execute().unwrap_or_report();
    }

    println!("{mm}");

    Ok(())
}

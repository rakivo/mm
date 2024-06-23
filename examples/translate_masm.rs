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
    let mut mm = Mm::from_masm(&input_file_path).unwrap();
    while !mm.halt() {
        mm.execute().unwrap();
    }

    println!("{mm}");

    Ok(())
}

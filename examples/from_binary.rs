use std::env;

extern crate mm;
use mm::*;

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("USAGE: {prog} <input>", prog = args[0]);
        return Ok(())
    }

    let file_path = &args[1];
    let mut mm = Mm::from_binary(file_path).unwrap();
    let mut i = 69;
    while i > 0 && !mm.halt() {
        mm.execute().unwrap();
        i -= 1;
        println!("{mm}");
    }
    Ok(())
}

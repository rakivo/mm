use std::{env, process::exit};

extern crate mm;
use mm::*;

fn find_flag(args: &Vec::<String>, flag: &str) -> Option::<String> {
    if let Some(pos) = args.iter().position(|x| x == flag) {
        if args.len() < pos + 1 {
            panic!("No expected argument for flag: {f}", f = args[pos]);
        } else if args.len() > pos + 1 {
            if args[pos + 1].starts_with('-') {
                panic!("Unxpected argument for flag: {f}: {a}", f = args[pos], a = args[pos + 1]);
            } else {
                Some(args[pos + 1].to_owned())
            }
        } else {
            None
        }
    } else { None }
}

pub fn native_test(mm: &mut Mm) {
    let stack = mm.stack_mut();
    stack.push_back(NaNBox::from_u64(69));
    stack.push_back(NaNBox::from_u64(420));
}

fn main() {
    let args = env::args().collect::<Vec<_>>();

    if args.len() < 2 {
        eprintln!("USAGE: {prog} <input> [-o <output>] [-l <limit>] [-d]", prog = args[0]);
        exit(1)
    }

    let input = &args[1];
    let natives = natives![native_test];
    let (mut mm, program) = Mm::try_from_masm(&input, vec![], natives).unwrap_or_report();

    let debug = args.contains(&"-d".to_owned());
    let output = find_flag(&args, "-o");
    let limit = find_flag(&args, "-l").map(|x| x.parse::<usize>().map_err(|err| {
        panic!("Failed to parse argument for flag: -l: {err}")
    }).unwrap());

    if let Some(out) = output {
        mm.to_binary(&out, &program).unwrap_or_report();
    } else {
        mm.execute_program(debug, limit, &program).unwrap_or_report();
    }
}

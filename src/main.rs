/// I'll do my best to keep the main.rs file clean.
/// Modularizing is good!

/// Struct/Enum for AST
mod ast;
/// Build KoopaIR from AST
mod builder;

use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};

lalrpop_mod!(sysy);

/// Usage: <program> <mode> <input> -o <output_file>
fn main() {
    let mut args = args();
    args.next(); // Ignore argv[0]
    let _mode = args.next().expect("missing argument <mode>");
    let input = args.next().expect("missing argument <input_file>");
    args.next(); // Ignore argv[3]
    let output = args.next().expect("missing argument <output_file>");

    let input = read_to_string(input).expect("cannot read input file");

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    println!("{:#?}", ast);

    let program = builder::build_program(ast).unwrap();
    builder::output_program(&program, File::create(output).unwrap());
}

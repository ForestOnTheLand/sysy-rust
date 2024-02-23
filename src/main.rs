/// I'll do my best to keep the main.rs file clean.
/// Modularizing is good!

/// Struct/Enum for AST
mod ast;
/// Build KoopaIR from AST
mod builder;

use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::fs::File;
use std::io::Result;

lalrpop_mod!(sysy);

/// Usage: <program> <mode> <input> -o <output_file>
fn main() -> Result<()> {
    let mut args = args();
    args.next(); // Ignore argv[0]
    let _mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next(); // Ignore argv[3]
    let output = args.next().unwrap();

    let input = read_to_string(input)?;

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    println!("{:#?}", ast);

    let program = builder::build_program(ast).unwrap();
    builder::output_program(&program, File::create(output).unwrap());

    Ok(())
}

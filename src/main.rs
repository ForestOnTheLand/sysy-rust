/// I'll do my best to keep the main.rs file clean.
/// Modularizing is good!

/// Struct/Enum for AST
mod ast;
/// Build KoopaIR from AST
mod builder;
/// Util functions
mod util;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(sysy);

/// Usage: <program> <mode> <input> -o <output_file>
fn main() {
    let (mode, input, output) = util::parse_args().unwrap();

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    println!("{:#?}", ast);

    let program = builder::build_program(&ast).unwrap();
    builder::output_program(&program, output);
}

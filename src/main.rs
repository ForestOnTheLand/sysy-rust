//! I'll do my best to keep the main.rs file clean.
//! Modularizing is good!

/// Struct/Enum for AST
mod ast;
/// Build KoopaIR from AST
mod builder;
/// RISCV Register
mod register;
/// Symbol table
mod symtab;
/// Translate KoopaIR into RISCV
mod translator;
/// Util functions
mod util;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(sysy);

/// Usage: `<program> <mode> <input> -o <output_file>`
fn main() {
    let (mode, input, mut output) = util::parse_args().unwrap();

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let program = builder::build_program(&ast);
    // println!("{:#?}", ast);
    match mode {
        util::Mode::Koopa => {
            builder::output_program(&program, output);
        }
        util::Mode::RiscV => {
            translator::translate_program(&program, &mut output);
        }
    }
}

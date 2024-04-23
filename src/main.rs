//! I'll do my best to keep the main.rs file clean.
//! Modularizing is good!

/// Struct/Enum for AST
mod ast;
/// Symbol table
mod build_util;
/// Build KoopaIR from AST
mod builder;
/// Optimizer
mod optimizer;
/// RISCV
mod riscv;
/// RISCV Register
mod translate_util;
/// Translate KoopaIR into RISCV
mod translator;
/// Util functions
mod util;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(sysy);

/// Usage: `<program> <mode> <input> -o <output_file>`
fn main() {
    koopa::ir::Type::set_ptr_size(4);
    let (mode, input, output) = util::parse_args();

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let program = builder::build_program(&ast);
    // println!("{:#?}", ast);
    match mode {
        util::Mode::Koopa => {
            builder::output_program(&program, output);
        }
        util::Mode::RiscV => {
            let code = translator::translate_program(&program);
            translator::output_program(&code, output);
        }
        util::Mode::Perf => {
            let mut code = translator::translate_program(&program);
            code.optimize();
            translator::output_program(&code, output);
        }
    }
}

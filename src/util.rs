//! Small tools
//!
//! including [`parse_args`] for parsing arguments

use std::{env, fs};

pub enum Mode {
    Koopa,
    RiscV,
    Perf,
}

pub fn print_usage() -> ! {
    let program = env::args().next().unwrap_or("<program>".to_string());
    panic!("Usage: {program} <mode> <input_file> -o <output_file>");
}

pub fn parse_args() -> (Mode, String, fs::File) {
    let args: Vec<String> = env::args().collect();
    let mode = match &args[1] as &str {
        "-koopa" => Mode::Koopa,
        "-riscv" => Mode::RiscV,
        "-perf" => Mode::Perf,
        _ => print_usage(),
    };
    let input = fs::read_to_string(args[2].clone()).unwrap();
    let output = fs::File::create(args[4].clone()).unwrap();
    (mode, input, output)
}

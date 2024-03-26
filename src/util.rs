//! Small tools, including
//! - [`parse_args`] for parsing arguments

use std::{env, fs, io};

#[derive(Debug)]
pub enum Error {
    MissingArgumentError,
    ModeError(String),
    IOError(io::Error),
    InternalError(String),
    ParseError(String),
}

pub enum Mode {
    Koopa,
    RiscV,
}

pub fn parse_args() -> Result<(Mode, String, fs::File), Error> {
    let mut args = env::args();
    args.next(); // Ignore argv[0]
    let mode = args.next().ok_or(Error::MissingArgumentError)?;
    let mode = match &mode as &str {
        "-koopa" => Mode::Koopa,
        "-riscv" | "-perf" => Mode::RiscV,
        _ => return Err(Error::ModeError(mode)),
    };
    let input = args.next().ok_or(Error::MissingArgumentError)?;
    let input = fs::read_to_string(input).map_err(Error::IOError)?;

    args.next(); // Ignore argv[3]
    let output = args.next().ok_or(Error::MissingArgumentError)?;
    let output = fs::File::create(output).map_err(Error::IOError)?;

    Ok((mode, input, output))
}

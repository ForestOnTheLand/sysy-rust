use std::{env, fs, io};

#[derive(Debug)]
pub enum Error {
    MissingArgument,
    InvalidArgument,
    OutputError(io::Error),
    InvalidFunctionName, // function name of KoopaIR should begin with @
    InvalidRegisterError,
    RegisterAllocError,
    RegisterFreeError,
    InternalError,
    NameError,
}

pub enum Mode {
    Koopa,
    RiscV,
}

pub fn parse_args() -> Result<(Mode, String, fs::File), Error> {
    let mut args = env::args();
    args.next(); // Ignore argv[0]
    let mode = args.next().ok_or(Error::MissingArgument)?;
    let mode = match &mode as &str {
        "-koopa" => Mode::Koopa,
        "-riscv" => Mode::RiscV,
        _ => return Err(Error::InvalidArgument),
    };
    let input = args.next().ok_or(Error::MissingArgument)?;
    let input = fs::read_to_string(input).map_err(Error::OutputError)?;

    args.next(); // Ignore argv[3]
    let output = args.next().ok_or(Error::MissingArgument)?;
    let output = fs::File::create(output).map_err(Error::OutputError)?;

    Ok((mode, input, output))
}

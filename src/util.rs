use std::{env, fmt, fs, io};

#[derive(Debug)]
pub enum Error {
    MissingArgumentError,
    ModeError(String),
    IOError(io::Error),
    InternalError(String),
    ParseError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::MissingArgumentError => {
                writeln!(f, "Usage: <program> <mode> <input-file> -o <output-file>")
            }
            Error::ModeError(mode) => {
                writeln!(
                    f,
                    r#"Unsupported mode '{}', expected in:
(1) -koopa: generates KoopaIR
(2) -riscv: generates RISCV"#,
                    mode
                )
            }
            Error::IOError(err) => {
                writeln!(f, "IO error: {}", err)
            }
            Error::InternalError(msg) => {
                writeln!(f, "internal error: {}", msg)
            }
            Error::ParseError(msg) => {
                writeln!(f, "parse error: {}", msg)
            }
        }
    }
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
        "-riscv" => Mode::RiscV,
        _ => return Err(Error::ModeError(mode)),
    };
    let input = args.next().ok_or(Error::MissingArgumentError)?;
    let input = fs::read_to_string(input).map_err(Error::IOError)?;

    args.next(); // Ignore argv[3]
    let output = args.next().ok_or(Error::MissingArgumentError)?;
    let output = fs::File::create(output).map_err(Error::IOError)?;

    Ok((mode, input, output))
}

use gumdrop::Options;
use std::{fmt, path::PathBuf, str::FromStr};

#[derive(Options)]
/// Compiles Lisp code into Scratch projects.
pub struct Opts {
    /// Display this help message
    pub help: bool,

    #[options(command)]
    pub command: Option<Command>,
}

#[derive(Options)]
pub enum Command {
    Compile(CompileOpts),
    Format(FormatOpts),
}

#[derive(Options)]
/// Compiles Lisp code into Scratch projects.
pub struct CompileOpts {
    /// Display this help message
    pub help: bool,

    /// The source file to compile
    #[options(free, required)]
    pub file: PathBuf,

    /// Type of code to compile to: sb3 (default) or x86_64
    pub target: Target,
}

#[derive(Default, Clone, Copy)]
pub enum Target {
    #[default]
    SB3,
    X86_64,
}

impl Target {
    pub const fn to_str(self) -> &'static str {
        match self {
            Self::SB3 => "sb3",
            Self::X86_64 => "x86_64",
        }
    }
}

impl FromStr for Target {
    type Err = InvalidTarget;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "sb3" => Ok(Self::SB3),
            "x86_64" => Ok(Self::X86_64),
            _ => Err(InvalidTarget(s.to_owned())),
        }
    }
}

pub struct InvalidTarget(String);

impl fmt::Display for InvalidTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid target: {}", self.0)
    }
}

#[derive(Options)]
/// Formats source code.
pub struct FormatOpts {
    /// Display this help message
    pub help: bool,
}

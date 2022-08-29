use gumdrop::Options;
use std::path::PathBuf;

#[derive(Options)]
/// Compiles Lisp code into Scratch projects.
pub struct Opts {
    /// Display this help message
    pub help: bool,

    /// The source file to compile
    #[options(free, required)]
    pub file: PathBuf,

    /// Run the linter while compiling
    #[options(no_short)]
    pub lint: bool,

    /// Dump the initial AST to a file
    #[options(no_short, meta = "FILE")]
    pub dump_ast: Option<PathBuf>,

    /// Dump the expanded AST to a file
    #[options(no_short, meta = "FILE")]
    pub dump_expanded: Option<PathBuf>,

    /// Dump the unoptimized IR to a file
    #[options(no_short, meta = "FILE")]
    pub dump_ir: Option<PathBuf>,

    /// Dump the optimized IR to a file
    #[options(no_short, meta = "FILE")]
    pub dump_optimized: Option<PathBuf>,
}

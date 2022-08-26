use gumdrop::Options;
use std::path::PathBuf;

#[derive(Options)]
/// Compiles Lisp code into Scratch projects.
pub struct Opts {
    /// Displays this help message
    pub help: bool,

    /// The source file to compile
    #[options(free, required)]
    pub file: PathBuf,

    /// Runs the linter while compiling
    #[options(no_short)]
    pub lint: bool,

    /// Dumps the initial AST to a file
    #[options(no_short, meta = "FILE")]
    pub dump_ast: Option<PathBuf>,

    /// Dumps the expanded AST to a file
    #[options(no_short, meta = "FILE")]
    pub dump_expanded: Option<PathBuf>,

    /// Dumps the unoptimized IR to a file
    #[options(no_short, meta = "FILE")]
    pub dump_ir: Option<PathBuf>,

    /// Dumps the optimized IR to a file
    #[options(no_short, meta = "FILE")]
    pub dump_optimized: Option<PathBuf>,
}

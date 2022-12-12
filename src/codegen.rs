mod sb3;

use crate::{
    diagnostic::Result,
    ir::Program,
    opts::{Opts, Target},
};
use std::path::Path;

pub fn write_program(program: &Program, opts: &Opts) -> Result<()> {
    match opts.target {
        Target::SB3 => sb3::write_sb3_file(program, Path::new("project.sb3")),
    }
}

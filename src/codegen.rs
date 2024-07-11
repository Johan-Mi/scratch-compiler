mod sb3;
mod x86_64;

use crate::{
    diagnostic::Result,
    ir::Program,
    opts::{self, Target},
};
use std::path::Path;

pub fn write_program(program: &Program, opts: &opts::Compile) -> Result<()> {
    match opts.target {
        Target::SB3 => sb3::write_sb3_file(program, Path::new("project.sb3")),
        Target::X86_64 => {
            x86_64::write_object_file(program, Path::new("project.o"))
        }
    }
}

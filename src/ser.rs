mod proc;
mod reporter;
mod sprite;

use crate::{
    asset::Asset,
    ir::{proc::CustomProcedure, Program},
    uid::{Uid, UidGenerator},
};
use serde_json::{json, Value as Json};
use smol_str::SmolStr;
use std::{
    cell::RefCell,
    collections::HashMap,
    fs::File,
    io::{self, BufWriter},
    iter,
    path::Path,
};
use zip::{write::FileOptions, ZipWriter};

pub fn write_sb3_file(program: &Program, path: &Path) {
    // TODO: Error handling
    let file = File::create(path).unwrap();
    let buf = BufWriter::new(file);
    let mut zip = ZipWriter::new(buf);
    zip.start_file("project.json", FileOptions::default())
        .unwrap();

    let uid_gen = UidGenerator::new();

    let global_vars = program
        .stage
        .variables
        .iter()
        .map(|var| {
            (
                var.into(),
                Mangled {
                    name: var.clone(),
                    id: uid_gen.new_uid(),
                },
            )
        })
        .collect::<HashMap<_, _>>();
    let global_lists = program
        .stage
        .lists
        .iter()
        .map(|list| {
            (
                list.into(),
                Mangled {
                    name: list.clone(),
                    id: uid_gen.new_uid(),
                },
            )
        })
        .collect::<HashMap<_, _>>();

    let mut ctx = SerCtx {
        uid_gen,
        blocks: RefCell::default(),
        custom_procs: HashMap::new(),
        proc_args: Vec::new(),
        local_vars: HashMap::new(),
        local_lists: HashMap::new(),
        sprite_vars: HashMap::new(),
        sprite_lists: HashMap::new(),
        global_vars,
        global_lists,
    };
    let targets = iter::once(("Stage", &program.stage))
        .chain(program.sprites.iter().map(|(name, spr)| (&**name, spr)))
        .map(|(name, spr)| ctx.serialize_sprite(name, spr))
        .collect::<Vec<_>>();

    serde_json::to_writer(
        &mut zip,
        &json!({
            "meta": {
                "semver": "3.0.0",
            },
            "targets": targets,
        }),
    )
    .unwrap();

    for (name, path) in program
        .sprites
        .values()
        .chain(iter::once(&program.stage))
        .flat_map(|sprite| &sprite.costumes)
    {
        let asset = Asset::new(name, path);
        let mut file = File::open(path).unwrap();
        zip.start_file(asset.md5ext, FileOptions::default())
            .unwrap();
        io::copy(&mut file, &mut zip).unwrap();
    }

    zip.finish().unwrap();
}

struct SerCtx {
    uid_gen: UidGenerator,
    blocks: RefCell<HashMap<Uid, Json>>,
    custom_procs: HashMap<SmolStr, CustomProcedure>,
    proc_args: Vec<SmolStr>,
    local_vars: HashMap<SmolStr, Mangled>,
    local_lists: HashMap<SmolStr, Mangled>,
    sprite_vars: HashMap<SmolStr, Mangled>,
    sprite_lists: HashMap<SmolStr, Mangled>,
    global_vars: HashMap<SmolStr, Mangled>,
    global_lists: HashMap<SmolStr, Mangled>,
}

impl SerCtx {
    fn new_uid(&self) -> Uid {
        self.uid_gen.new_uid()
    }
}

struct Mangled {
    name: String,
    id: Uid,
}

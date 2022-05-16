mod proc;

use self::proc::serialize_procs;
use crate::{
    asset::asset_json,
    ir::{sprite::Sprite, Program},
    uid::{Uid, UidGenerator},
};
use serde_json::{json, Value as Json};
use std::{fs::File, iter, path::Path};
use zip::{write::FileOptions, ZipWriter};

pub(crate) fn write_sb3_file(program: &Program, path: &Path) {
    // TODO: Error handling
    let file = File::create(path).unwrap();
    let mut zip = ZipWriter::new(file);
    zip.start_file("project.json", FileOptions::default())
        .unwrap();

    let mut ctx = ProgramCtx {
        uid_gen: UidGenerator::new(),
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
    zip.finish().unwrap();
}

struct ProgramCtx {
    uid_gen: UidGenerator,
}

impl ProgramCtx {
    fn new_uid(&self) -> Uid {
        self.uid_gen.new_uid()
    }
}

impl ProgramCtx {
    fn serialize_sprite(&self, name: &str, sprite: &Sprite) -> Json {
        let variables = sprite
            .variables
            .iter()
            .map(|var| Mangled {
                source_name: var.clone(),
                mangled_name: var.clone(),
                id: self.new_uid(),
            })
            .collect::<Vec<_>>();
        let lists = sprite
            .lists
            .iter()
            .map(|lst| Mangled {
                source_name: lst.clone(),
                mangled_name: lst.clone(),
                id: self.new_uid(),
            })
            .collect::<Vec<_>>();
        let costumes = sprite
            .costumes
            .iter()
            .map(|(name, path)| asset_json(name, path))
            .collect::<Vec<_>>();
        let blocks = serialize_procs(self, &sprite.procedures);

        json!({
            "name": name,
            "isStage": name == "Stage",
            "variables": variables.iter().map(|var| (&var.mangled_name, 0)).collect::<Vec<_>>(),
            "lists": lists.iter().map(|var| (&var.mangled_name, [] as [(); 0])).collect::<Vec<_>>(),
            "costumes": costumes,
            "currentCostume": 1,
            "sounds": [],
            "blocks": blocks,
        })
    }
}

struct Mangled {
    source_name: String,
    mangled_name: String,
    id: Uid,
}

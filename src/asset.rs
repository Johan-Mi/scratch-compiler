use serde_json::{json, Value as Json};
use std::{fs::File, io::Read, path::Path};

pub(crate) fn asset_json(name: &str, path: &Path) -> Json {
    // TODO: Error handling
    let mut file = File::open(path).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();
    let md5_sum = md5::compute(&buf);
    let extension = path.extension().unwrap().to_str().unwrap();

    json!({
        "assetId": format!("{md5_sum:x}"),
        "name": name,
        "md5ext": format!("{md5_sum:x}.{extension}"),
        "dataFormat": extension,
    })
}

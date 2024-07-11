use serde::Serialize;
use std::path::Path;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Asset {
    asset_id: String,
    name: String,
    pub md5ext: String,
    data_format: String,
}

impl Asset {
    pub fn new(name: &str, path: &Path) -> Self {
        // TODO: Error handling
        let buf = std::fs::read(path).unwrap();
        let md5_sum = md5::compute(buf);
        let extension = path.extension().unwrap().to_str().unwrap().to_owned();

        Self {
            asset_id: format!("{md5_sum:x}"),
            name: name.to_owned(),
            md5ext: format!("{md5_sum:x}.{extension}"),
            data_format: extension,
        }
    }
}

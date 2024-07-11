use serde::Serialize;
use std::path::Path;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Asset {
    #[serde(rename = "assetId")]
    id: String,
    name: String,
    pub md5ext: String,
    data_format: String,
}

impl Asset {
    pub fn new(name: String, path: &Path) -> Self {
        // TODO: Error handling
        let buf = std::fs::read(path).unwrap();
        let md5_sum = md5::compute(buf);
        let extension = path.extension().unwrap().to_str().unwrap().to_owned();

        Self {
            id: format!("{md5_sum:x}"),
            name,
            md5ext: format!("{md5_sum:x}.{extension}"),
            data_format: extension,
        }
    }
}

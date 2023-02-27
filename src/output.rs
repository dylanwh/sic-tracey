use std::{
    fs::{create_dir_all, write},
    path::PathBuf,
};

use eyre::Result;
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use walkdir::{DirEntry, WalkDir};

// this is used as a prefix by strace.
const STRACE_LOG_PREFIX: &str = "strace.log";
const FILE_TRACER_METADATA: &str = "file_tracer.json";

#[derive(Debug, Serialize, Deserialize)]
pub struct TraceOutput {
    pub base_dir: PathBuf,
    pub program: Vec<String>,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub uuid: uuid::Uuid,
}

impl TraceOutput {
    pub fn create<P>(output_dir: P, program: &[String]) -> Result<Self>
    where
        P: AsRef<std::path::Path>,
    {
        let timestamp = chrono::Utc::now();
        let uuid = Uuid::now_v7();
        let output = Self {
            base_dir: PathBuf::from(output_dir.as_ref()),
            program: program.to_vec(),
            timestamp,
            uuid,
        };

        // return error if directory already exists
        let dir = output.dir();
        if dir.exists() {
            return Err(eyre::eyre!("directory already exists: {:?}", output.dir()));
        }
        create_dir_all(&dir)?;
        // write json metadata file
        let metadata = serde_json::to_string_pretty(&output)?;
        write(output.metadata_file(), metadata)?;

        Ok(output)
    }

    pub fn open<P>(dir: P) -> Result<Self>
    where
        P: AsRef<std::path::Path>,
    {
        let file = std::fs::File::open(dir.as_ref().join(FILE_TRACER_METADATA))?;
        let reader = std::io::BufReader::new(file);
        let output = serde_json::from_reader(reader)?;
        Ok(output)
    }

    pub fn dir(&self) -> PathBuf {
        self.base_dir.join(self.uuid.as_simple().to_string())
    }

    pub fn metadata_file(&self) -> PathBuf {
        self.dir().join(FILE_TRACER_METADATA)
    }

    pub fn strace_file(&self) -> Result<String> {
        self.dir()
            .join(STRACE_LOG_PREFIX)
            .to_str()
            .map(|s| s.to_string())
            .ok_or(eyre::eyre!("invalid path"))
    }

    pub fn strace_files(&self) -> impl Iterator<Item = DirEntry> {
        WalkDir::new(self.dir())
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
            .filter(|e| {
                e.file_name()
                    .to_str()
                    .unwrap_or_default()
                    .starts_with(STRACE_LOG_PREFIX)
            })
    }

    // list all directories that contain a metadata file, sorted by timestamp
    pub fn list(output_dir: &str) -> Result<Vec<PathBuf>> {
        let mut dirs = WalkDir::new(output_dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_dir())
            // the dir has a metadata file
            .filter(|e| e.path().join(FILE_TRACER_METADATA).exists())
            .map(|e| e.path().to_path_buf())
            .collect::<Vec<_>>();

        dirs.sort();

        Ok(dirs)
    }
}

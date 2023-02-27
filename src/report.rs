use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use derive_visitor::Visitor;

use crate::parser_model::{Annotation, TraceArg};

#[derive(Debug, Default, Visitor)]
#[visitor(Annotation(enter), TraceArg(enter))]
pub struct Report {
    pub files: HashSet<PathBuf>,
}

impl Report {
    fn enter_annotation(&mut self, annotation: &Annotation) {
        self.insert(annotation.file());
    }

    fn enter_trace_arg(&mut self, arg: &TraceArg) {
        match arg {
            TraceArg::String {
                value,
                truncated: false,
            } => {
                self.insert(value);
            }
            TraceArg::Array { values, .. } => {
                for value in values {
                    if let TraceArg::String {
                        value,
                        truncated: false,
                    } = value
                    {
                        self.insert(value);
                    }
                }
            }
            _ => {}
        }
    }

    fn insert<P>(&mut self, path: P)
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        if !path.is_absolute() {
            return;
        }
        if let Ok(m) = path.metadata() {
            if m.is_file() || m.is_dir() || m.is_symlink() {
                self.files.insert(path.to_owned());
            }
            // also insert the target of symlinks
            if m.is_symlink() {
                if let Ok(target) = path.read_link() {
                    self.insert(target);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use derive_visitor::Drive;

    use crate::parser_model::{Trace, TraceLog, TraceReturn};

    use super::*;

    #[test]
    fn test_report() {
        let mut report = Report::default();
        let annotation = Some(Annotation::File("/usr/local/bin/test".to_string()));
        let value: TraceArg = TraceArg::Integer {
            value: 0,
            annotation,
        };
        let constant = None;
        let comment = None;
        let duration = 0.0;

        let trace = Trace::Call {
            time: 0.0,
            name: "open".to_string(),
            args: vec![],
            return_value: TraceReturn::Normal {
                value,
                constant,
                comment,
                duration,
            },
        };
        let trace_log = TraceLog {
            traces: vec![trace],
        };
        trace_log.drive(&mut report);

        assert_eq!(report.files.len(), 1);
        assert!(report.files.contains(&PathBuf::from("/usr/local/bin/test")));
    }
}

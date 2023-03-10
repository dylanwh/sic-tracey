use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use derive_visitor::Visitor;

use crate::parser_model::{Annotation, Trace, TraceArg};

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
        let ignore = path.starts_with("/proc")
            || path.starts_with("/sys")
            || path.starts_with("/dev")
            || path.starts_with("/run")
            || path.starts_with("/tmp")
            || !path.is_absolute();
        if ignore {
            return;
        }
        self.files.insert(path.to_path_buf());
    }
}

#[derive(Debug, Default, Visitor)]
#[visitor(Trace(enter))]
pub struct JunkReport {
    pub junk: HashSet<String>,
}

impl JunkReport {
    fn enter_trace(&mut self, trace: &Trace) {
        if let Trace::Junk(line) = trace {
            self.junk.insert(line.to_string());
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
        let annotation = Annotation::File("/bin/sh".to_string());
        let value: TraceArg = TraceArg::Integer {
            value: 0,
            annotation,
        };
        let constant = None;
        let comment = None;
        let duration = Some(0.0);

        let trace = Trace::Call {
            time: 0.0,
            name: "open".to_string(),
            args: vec![],
            return_value: TraceReturn {
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
        assert!(report.files.contains(&PathBuf::from("/bin/sh")));
    }
}

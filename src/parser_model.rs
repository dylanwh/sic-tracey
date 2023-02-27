use derive_visitor::Drive;

#[derive(Debug, Drive)]
pub struct TraceLog {
    pub traces: Vec<Trace>,
}

#[derive(Debug, PartialEq, Drive)]
pub enum Trace {
    Call {
        time: f64,
        name: String,
        args: Vec<TraceArg>,
        return_value: TraceReturn,
    },

    Exit {
        time: f64,
        status: i32,
    },
}

#[derive(Debug, PartialEq, Drive)]
pub enum TraceArg {
    Identifier {
        value: String,
        annotation: Option<Annotation>,
    },
    String {
        value: String,
        truncated: bool,
    },
    Array {
        values: Vec<TraceArg>,
        truncated: bool,
    },
    Struct {
        fields: Vec<(String, TraceArg)>,
        truncated: bool,
    },
    Integer {
        value: i64,
        annotation: Option<Annotation>,
    },
    HexInteger {
        value: u64,
        comment: Option<String>,
    },
    OctInteger(u64),
    BitwiseOr(Box<TraceArg>, Box<TraceArg>),
    Mul(Box<TraceArg>, Box<TraceArg>),
    List {
        args: Vec<TraceArg>,
        truncated: bool,
    },
    Fun {
        name: String,
        args: Vec<TraceArg>,
    },
}

impl TraceArg {
    #[cfg(test)]
    pub fn string(s: &str) -> Self {
        TraceArg::String {
            value: s.to_string(),
            truncated: false,
        }
    }

    #[cfg(test)]
    pub fn truncated_string(s: &str) -> Self {
        TraceArg::String {
            value: s.to_string(),
            truncated: true,
        }
    }

    #[cfg(test)]
    pub fn id(s: &str) -> Self {
        TraceArg::Identifier {
            value: s.to_string(),
            annotation: None,
        }
    }

    #[cfg(test)]
    pub fn annotate<S>(&self, annotation: S) -> Self
    where
        S: Into<String>,
    {
        let annotation = annotation.into();
        match self {
            Self::Identifier { value, .. } => TraceArg::Identifier {
                value: value.clone(),
                annotation: Some(Annotation::File(annotation)),
            },
            Self::Integer { value, .. } => TraceArg::Integer {
                value: *value,
                annotation: Some(Annotation::File(annotation)),
            },
            _ => panic!("cannot annotate {:?}", self),
        }
    }

    #[cfg(test)]
    pub fn integer(arg: i64) -> TraceArg {
        Self::Integer {
            value: arg,
            annotation: None,
        }
    }
}

#[derive(Debug, PartialEq, Drive)]
pub enum TraceReturn {
    Normal {
        value: TraceArg,
        constant: Option<String>,
        comment: Option<String>,
        duration: f64,
    },
    Unknown, // ?
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash, Drive)]
pub enum Annotation {
    File(String),
    CharDevice(String, u32, u32),
}

impl Annotation {
    pub fn file(&self) -> &str {
        match self {
            Self::File(path) => path,
            Self::CharDevice(path, _, _) => path,
        }
    }
}

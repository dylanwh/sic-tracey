use derive_visitor::Drive;
use serde::Serialize;

#[derive(Debug, Drive, Serialize)]
pub struct TraceLog {
    pub traces: Vec<Trace>,
}

#[derive(Debug, PartialEq, Drive, Serialize)]
pub enum Trace {
    Call {
        #[drive(skip)]
        time: f64,

        #[drive(skip)]
        name: String,

        args: Vec<TraceArg>,
        return_value: TraceReturn,
    },

    Exit {
        #[drive(skip)]
        time: f64,

        #[drive(skip)]
        status: i32,
    },

    #[drive(skip)]
    Junk(String),
}

#[derive(Debug, PartialEq, Drive, Serialize)]
pub struct Field(#[drive(skip)] pub String, pub TraceArg);

#[derive(Debug, PartialEq, Drive, Serialize)]
pub enum TraceArg {
    Identifier {
        #[drive(skip)]
        value: String,
        annotation: Annotation,
    },
    String {
        #[drive(skip)]
        value: String,

        #[drive(skip)]
        truncated: bool,
    },
    Array {
        values: Vec<TraceArg>,

        #[drive(skip)]
        truncated: bool,
    },
    Struct {
        fields: Vec<Field>,

        #[drive(skip)]
        truncated: bool,
    },
    Integer {
        #[drive(skip)]
        value: i64,

        annotation: Annotation,
    },
    HexInteger {
        #[drive(skip)]
        value: u64,

        #[drive(skip)]
        comment: Option<String>,
    },

    #[drive(skip)]
    OctInteger(u64),

    BitwiseOr(Box<TraceArg>, Box<TraceArg>),

    Mul(Box<TraceArg>, Box<TraceArg>),

    #[drive(skip)]
    Field(String, Box<TraceArg>),

    List {
        args: Vec<TraceArg>,

        #[drive(skip)]
        truncated: bool,
    },
    Fun {
        #[drive(skip)]
        name: String,

        #[drive(skip)]
        args: Vec<TraceArg>,
    },
    Unknown,
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
            annotation: Annotation::default(),
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
                annotation: Annotation::File(annotation),
            },
            Self::Integer { value, .. } => TraceArg::Integer {
                value: *value,
                annotation: Annotation::File(annotation),
            },
            _ => panic!("cannot annotate {:?}", self),
        }
    }

    #[cfg(test)]
    pub fn integer(arg: i64) -> TraceArg {
        Self::Integer {
            value: arg,
            annotation: Annotation::default(),
        }
    }
}

#[derive(Debug, PartialEq, Drive, Serialize)]
pub struct TraceReturn {
    pub value: TraceArg,
    #[drive(skip)]
    pub constant: Option<String>,

    #[drive(skip)]
    pub comment: Option<String>,

    #[drive(skip)]
    pub duration: Option<f64>,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash, Drive, Serialize, Default)]
pub enum Annotation {
    #[drive(skip)]
    File(String),

    #[drive(skip)]
    CharDevice(String, u32, u32),

    #[default]
    Empty,
}

impl Annotation {
    pub fn file(&self) -> &str {
        match self {
            Self::File(path) => path,
            Self::CharDevice(path, _, _) => path,
            Self::Empty => "",
        }
    }
}

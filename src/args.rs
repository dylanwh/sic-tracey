use clap::Parser;
// parse args with clap, deriving from the structure Options
#[derive(Clone, Debug, Parser)]
pub struct Args {
    /// output directory
    /// default: ./strace_logs
    #[clap(short, long, default_value = "./strace_logs")]
    pub output_dir: String,

    #[command(subcommand)]
    pub action: Action,
}

#[derive(Clone, Debug, clap::Subcommand)]
pub enum Action {
    /// run a program with strace. Note that no additional output is printed to stdout/stderr.
    /// The output is written to a file in the output directory.
    Trace {
        /// string limit, specify the maximum length of a string to be printed. Defaults to strace's default of 32.
        #[clap(short = 's', long)]
        string_limit: Option<usize>,

        /// program to run, rest of the args
        /// required.
        #[clap(required = true)]
        program: Vec<String>,
    },

    /// List uuids of all strace logs in the output directory, one per line.
    List,

    /// Show junk lines from a strace log by uuid. Only a unique subset of the uuid is required.
    Junk {
        #[clap(default_value = "latest")]
        uuid: String,
    },

    /// Show a strace log by uuid. Only a unique subset of the uuid is required.
    Files {
        #[clap(default_value = "latest")]
        uuid: String,
    },

    /// Parse a file and print the output to stdout.
    Parse {
        /// file to parse
        /// required.
        #[clap(required = true)]
        file: String,
    },
}

impl Args {
    pub fn new() -> Self {
        Args::parse()
    }
}

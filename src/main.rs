mod args;
mod output;
mod parser;
mod parser_model;
mod report;

use std::process::ExitCode;

use args::*;
use derive_visitor::Drive;
use eyre::Result;
use output::*;
use report::{JunkReport, Report};

pub const STRACE_OPTIONS: &str = "-ff -y -yy -T -ttt";

fn main() -> Result<ExitCode> {
    let args = Args::new();

    match args.action.clone() {
        Action::Trace {
            string_limit,
            program,
        } => trace(&args, string_limit, program),

        Action::Files { uuid } => files(&args, &uuid),

        Action::List => list(&args),

        Action::Junk { uuid } => junk(&args, &uuid),

        Action::Parse { file } => parse(&args, &file),
    }
}

fn junk(args: &Args, uuid: &str) -> Result<ExitCode> {
    let output = if uuid == "latest" {
        TraceOutput::find_latest(&args.output_dir)
    } else {
        TraceOutput::find(&args.output_dir, uuid)
    }?;
    let mut report = JunkReport::default();
    for log in output.strace_files() {
        let trace_log = parser::parse_file(log.path())?;
        trace_log.drive(&mut report);
    }
    for junk in report.junk {
        println!("{}", junk);
    }

    Ok(ExitCode::SUCCESS)
}

fn files(args: &Args, uuid: &str) -> Result<ExitCode> {
    let output = if uuid == "latest" {
        TraceOutput::find_latest(&args.output_dir)
    } else {
        TraceOutput::find(&args.output_dir, uuid)
    }?;
    let mut report = Report::default();
    for log in output.strace_files() {
        let trace_log = parser::parse_file(log.path())?;
        trace_log.drive(&mut report);
    }
    for file in report.files {
        println!("{}", file.to_string_lossy());
    }

    Ok(ExitCode::SUCCESS)
}

fn trace(args: &Args, string_limit: Option<usize>, program: Vec<String>) -> eyre::Result<ExitCode> {
    let output = TraceOutput::create(&args.output_dir, &program)?;
    let mut strace_args = vec!["-o".to_string(), output.strace_file()?];
    strace_args.append(
        &mut STRACE_OPTIONS
            .split_whitespace()
            .map(|s| s.to_string())
            .collect(),
    );
    if let Some(string_limit) = string_limit {
        strace_args.push(format!("-s{}", string_limit));
    }
    let mut program = program;
    strace_args.append(&mut program);

    let mut strace = std::process::Command::new("strace")
        .args(strace_args)
        .spawn()?;

    let status = strace.wait()?;

    if !status.success() {
        let code = status.code().unwrap_or(1) as u8;
        return Ok(ExitCode::from(code));
    }

    Ok(ExitCode::SUCCESS)
}

fn list(args: &Args) -> Result<ExitCode> {
    for dir in TraceOutput::list(&args.output_dir)? {
        let output = TraceOutput::open(dir)?;
        println!("{} ({})", output.uuid.simple(), output.program.join(" "));
    }
    Ok(ExitCode::SUCCESS)
}

fn parse(_args: &Args, file: &str) -> Result<ExitCode> {
    let trace_log = parser::parse_file(file)?;
    println!("{}", serde_json::to_string_pretty(&trace_log)?);
    Ok(ExitCode::SUCCESS)
}

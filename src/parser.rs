use crate::parser_model::*;

use nom::{
    bytes::{complete::take_until, streaming::tag},
    character::{
        complete::{self, none_of, one_of},
        is_alphanumeric, is_digit,
        streaming::{self, multispace0, space0},
    },
    combinator::opt,
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, preceded, terminated},
    IResult,
};
use std::str::FromStr;

pub fn parse_file<P>(file: P) -> eyre::Result<TraceLog>
where
    P: AsRef<std::path::Path>,
{
    let buf = std::fs::read_to_string(file.as_ref())?;
    let (_, trace_log) =
        parse_lines(&buf).map_err(|e| eyre::eyre!("failed to parse file: {e:?}"))?;
    Ok(trace_log)
}

fn parse_lines(input: &str) -> IResult<&str, TraceLog> {
    let (input, traces) = separated_list1(tag("\n"), parse_line)(input)?;
    let (input, _) = opt(tag("\n"))(input)?;

    Ok((input, TraceLog { traces }))
}

fn parse_line(input: &str) -> IResult<&str, Trace> {
    if input.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Eof,
        )));
    }
    let (input, trace) = nom::branch::alt((parse_call, parse_exit, parse_junk))(input)?;
    Ok((input, trace))
}

pub fn parse_call(input: &str) -> IResult<&str, Trace> {
    let (input, time) = parse_float::<f64>(input)?;
    let (input, name) = preceded(multispace0, parse_identifier)(input)?;
    let (input, args) = parse_args(input)?;

    let (input, return_value) = preceded(
        ws(tag("=")),
        nom::branch::alt((parse_unknown_return, parse_return)),
    )(input)?;
    let call = Trace::Call {
        time,
        name,
        args,
        return_value,
    };
    Ok((input, call))
}

fn parse_exit(input: &str) -> IResult<&str, Trace> {
    let (input, time) = parse_float::<f64>(input)?;
    let (input, _) = tag(" +++ exited with ")(input)?;
    let (input, exit_code) = parse_signed_int::<i32>(input)?;
    let (input, _) = tag(" +++")(input)?;
    Ok((
        input,
        Trace::Exit {
            time,
            status: exit_code,
        },
    ))
}

fn parse_junk(input: &str) -> IResult<&str, Trace> {
    let (input, junk) = take_until("\n")(input)?;
    Ok((input, Trace::Junk(junk.to_string())))
}

// -1 ENOENT (No such file or directory)
fn parse_return(input: &str) -> IResult<&str, TraceReturn> {
    let (input, value) = nom::branch::alt((parse_arg_hex, parse_arg_int))(input)?;
    let (input, constant) = opt(preceded(space0, parse_identifier))(input)?;
    let (input, comment) = opt(preceded(space0, parse_comment))(input)?;
    let (input, duration) = preceded(
        multispace0,
        delimited(tag("<"), parse_float::<f64>, tag(">")),
    )(input)?;
    let return_value = TraceReturn::Normal {
        value,
        constant,
        comment,
        duration,
    };
    Ok((input, return_value))
}

fn parse_unknown_return(input: &str) -> IResult<&str, TraceReturn> {
    let (input, _) = tag("?")(input)?;
    Ok((input, TraceReturn::Unknown))
}

fn octal_escape(input: &str) -> IResult<&str, char> {
    let (input, _) = tag("\\")(input)?;
    let (input, d1) = one_of("01234567")(input)?;
    let (input, d2) = one_of("01234567")(input)?;
    let octal_num = format!("{}{}", d1, d2);
    let octal_num = u8::from_str_radix(&octal_num, 8).expect("failed to parse octal number");
    Ok((input, octal_num as char))
}

fn parse_annotation(input: &str) -> IResult<&str, Annotation> {
    let annotation_char = nom::branch::alt((none_of("\\<>"), octal_escape));
    let (input, file) = preceded(tag("<"), many0(annotation_char))(input)?;
    let file = file.into_iter().collect::<String>();

    // next it will either be > or <char 136:12>>
    let (input, chardev) = opt(parse_char_dev_annotation)(input)?;
    let (input, _) = tag(">")(input)?;

    let annotation = match chardev {
        Some((major, minor)) => Annotation::CharDevice(file, major, minor),
        None => Annotation::File(file),
    };

    Ok((input, annotation))
}

// <char 136:12>
fn parse_char_dev_annotation(input: &str) -> IResult<&str, (u32, u32)> {
    let (input, _) = tag("<char")(input)?;
    let (input, _) = space0(input)?;
    let (input, major) = parse_unsigned_int::<u32>(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, minor) = parse_unsigned_int::<u32>(input)?;
    let (input, _) = tag(">")(input)?;
    Ok((input, (major, minor)))
}

fn parse_signed_int<F>(input: &str) -> IResult<&str, F>
where
    F: FromStr,
{
    let (input, neg) = opt(tag("-"))(input)?;
    let (input, value) = complete::digit1(input)?;
    let neg = neg.unwrap_or("");
    let value = format!("{}{}", neg, value);
    match value.parse::<F>() {
        Ok(num) => Ok((input, num)),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Digit,
        ))),
    }
}

fn parse_unsigned_int<F>(input: &str) -> IResult<&str, F>
where
    F: FromStr,
{
    let (input, value) = complete::digit1(input)?;
    match value.parse::<F>() {
        Ok(num) => Ok((input, num)),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Digit,
        ))),
    }
}

fn parse_float<F>(input: &str) -> IResult<&str, F>
where
    F: FromStr,
{
    let (input, neg) = opt(tag("-"))(input)?;
    let (input, int_val) = opt(complete::digit1)(input)?;
    let (input, dec_val) = opt(preceded(tag("."), complete::digit1))(input)?;
    let neg = neg.unwrap_or("");
    let int_val = int_val.unwrap_or("0");
    let dec_val = dec_val.unwrap_or("0");
    let value = format!("{}{}.{}", neg, int_val, dec_val);
    match value.parse::<F>() {
        Ok(num) => Ok((input, num)),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Digit,
        ))),
    }
}

fn parse_comment(input: &str) -> IResult<&str, String> {
    let (input, comment) = delimited(tag("("), take_until(")"), tag(")"))(input)?;

    Ok((input, comment.to_string()))
}

pub fn parse_args(input: &str) -> IResult<&str, Vec<TraceArg>> {
    let (input, args) =
        delimited(tag("("), separated_list0(ws(tag(",")), parse_arg), tag(")"))(input)?;
    Ok((input, args))
}

fn parse_arg(input: &str) -> IResult<&str, TraceArg> {
    let (input, arg) = nom::branch::alt((
        parse_arg_array,
        parse_arg_list,
        parse_arg_struct,
        parse_arg_fun,
        parse_arg_expr,
    ))(input)?;
    Ok((input, arg))
}

fn parse_arg_expr(input: &str) -> IResult<&str, TraceArg> {
    // constant, integer, string, oct, hex
    let (input, arg) = nom::branch::alt((
        parse_arg_string,
        parse_arg_constant,
        parse_arg_hex,
        parse_arg_oct,
        parse_arg_int,
    ))(input)?;
    if input.is_empty() {
        return Ok((input, arg));
    }

    let (input, op) = opt(ws(one_of("*|")))(input)?;

    match op {
        Some('|') => {
            let (input, arg2) = parse_arg_expr(input)?;
            Ok((input, TraceArg::BitwiseOr(Box::new(arg), Box::new(arg2))))
        }
        Some('*') => {
            let (input, arg2) = parse_arg_expr(input)?;
            Ok((input, TraceArg::Mul(Box::new(arg), Box::new(arg2))))
        }
        _ => Ok((input, arg)),
    }
}

fn ws<'a, F, O, E>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: nom::Parser<&'a str, O, E>,
    E: nom::error::ParseError<&'a str>,
{
    delimited(multispace0, inner, multispace0)
}

fn parse_arg_array(input: &str) -> IResult<&str, TraceArg> {
    let (input, _) = ws(tag("["))(input)?;
    let (input, values) = separated_list0(ws(tag(",")), parse_arg)(input)?;
    let (input, _) = opt(ws(tag(",")))(input)?;
    let (input, truncated) = opt(tag("..."))(input)?;
    let (input, _) = tag("]")(input)?;

    Ok((
        input,
        TraceArg::Array {
            values,
            truncated: truncated.is_some(),
        },
    ))
}

fn parse_arg_fun(input: &str) -> IResult<&str, TraceArg> {
    let (input, _) = ws(tag("makedev"))(input)?;
    let (input, args) = parse_args(input)?;
    Ok((
        input,
        TraceArg::Fun {
            name: "makedev".to_string(),
            args,
        },
    ))
}

fn parse_arg_list(input: &str) -> IResult<&str, TraceArg> {
    let (input, _) = ws(tag("{"))(input)?;
    let (input, args) = separated_list1(streaming::multispace1, parse_arg)(input)?;
    let (input, truncated) = opt(ws(tag("...")))(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((
        input,
        TraceArg::List {
            args,
            truncated: truncated.is_some(),
        },
    ))
}

fn parse_arg_struct(input: &str) -> IResult<&str, TraceArg> {
    let (input, _) = ws(tag("{"))(input)?;
    let (input, fields) = separated_list0(ws(tag(",")), parse_struct_field)(input)?;
    let (input, _) = opt(ws(tag(",")))(input)?;
    let (input, truncated) = opt(tag("..."))(input)?;
    let (input, _) = tag("}")(input)?;

    let truncated = truncated.is_some();

    Ok((input, TraceArg::Struct { fields, truncated }))
}

// as in st_mode=S_IFDIR|0775
fn parse_struct_field(input: &str) -> IResult<&str, (String, TraceArg)> {
    let (input, field) = parse_identifier(input)?;
    let (input, _) = ws(tag("="))(input)?;
    let (input, value) = parse_arg(input)?;
    Ok((input, (field, value)))
}

fn parse_arg_oct(input: &str) -> IResult<&str, TraceArg> {
    let (input, value) = preceded(tag("0"), streaming::digit1)(input)?;
    let value = u64::from_str_radix(value, 8);
    match value {
        Ok(value) => Ok((input, TraceArg::OctInteger(value))),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Digit,
        ))),
    }
}

fn parse_arg_hex(input: &str) -> IResult<&str, TraceArg> {
    let (input, value) = preceded(tag("0x"), nom::character::complete::hex_digit1)(input)?;
    let value = u64::from_str_radix(value, 16).map_err(|_| {
        nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Digit))
    })?;
    let annotation: IResult<&str, &str> = preceded(
        multispace0,
        delimited(tag("/*"), take_until("*/"), tag("*/")),
    )(input);
    match annotation {
        Ok((input, annotation)) => Ok((
            input,
            TraceArg::HexInteger {
                value,
                comment: Some(annotation.trim().into()),
            },
        )),
        Err(_) => Ok((
            input,
            TraceArg::HexInteger {
                value,
                comment: None,
            },
        )),
    }
}

// an integer is either a bare integer or an integer with an annotation
fn parse_arg_int(input: &str) -> IResult<&str, TraceArg> {
    let (input, value) = parse_signed_int::<i64>(input)?;
    let (input, annotation) = opt(parse_annotation)(input)?;
    Ok((input, TraceArg::Integer { value, annotation }))
}

fn parse_arg_string(input: &str) -> IResult<&str, TraceArg> {
    let qs = preceded(tag("\""), in_quotes);
    let (input, s) = terminated(qs, tag("\""))(input)?;

    // match "..." or backtrack
    let r: IResult<&str, &str> = tag("...")(input);
    match r {
        Ok((input, _)) => Ok((
            input,
            TraceArg::String {
                value: s,
                truncated: true,
            },
        )),
        Err(_) => Ok((
            input,
            TraceArg::String {
                value: s,
                truncated: false,
            },
        )),
    }
}

fn parse_arg_constant(input: &str) -> IResult<&str, TraceArg> {
    let (input, id) = parse_identifier(input)?;
    let (input, annotation) = opt(parse_annotation)(input)?;
    Ok((
        input,
        TraceArg::Identifier {
            value: id,
            annotation,
        },
    ))
}

fn parse_identifier(input: &str) -> IResult<&str, String> {
    // let (input2, id) = take_while1(is_ident_char)(input)?;
    // streaming version
    let (input2, id) = nom::bytes::streaming::take_while1(is_ident_char)(input)?;
    match id.chars().next() {
        Some(ch) if is_digit(ch as u8) => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Digit,
        ))),
        _ => Ok((input2, id.into())),
    }
}

fn is_ident_char(ch: char) -> bool {
    is_alphanumeric(ch as u8) || ch == '_'
}

fn in_quotes(buf: &str) -> IResult<&str, String> {
    let mut ret = String::new();
    let mut skip_delimiter = false;
    for (i, ch) in buf.char_indices() {
        if ch == '\\' && !skip_delimiter {
            skip_delimiter = true;
        } else if ch == '"' && !skip_delimiter {
            return Ok((&buf[i..], ret));
        } else {
            ret.push(ch);
            skip_delimiter = false;
        }
    }
    Err(nom::Err::Incomplete(nom::Needed::Unknown))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_truncated_string() {
        let input = "\"foo\"...";
        let expected = TraceArg::String {
            value: "foo".to_string(),
            truncated: true,
        };
        let actual = parse_arg_string(input).unwrap().1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_truncated_string_with_escape() {
        let input = "\"foo\\\"bar\"...";
        let expected = TraceArg::String {
            value: "foo\"bar".to_string(),
            truncated: true,
        };
        let actual = parse_arg_string(input).unwrap().1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_string() {
        let input = "\"foo\"";
        let expected = TraceArg::String {
            value: "foo".to_string(),
            truncated: false,
        };
        let actual = parse_arg_string(input).unwrap().1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_arg_constant_1() {
        let input = "(foo)";
        let expected = vec![TraceArg::id("foo")];
        let r = parse_args(input);
        let actual = r.unwrap().1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_arg_constant_2() {
        let input = "(AT_FDCWD</home/dylan/Git/dylanwh/file-tracer>)";
        let expected =
            vec![TraceArg::id("AT_FDCWD").annotate("/home/dylan/Git/dylanwh/file-tracer")];
        let actual = parse_args(input).unwrap().1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_arg() {
        let input = r#""foo""#;
        let expected = TraceArg::String {
            value: "foo".to_string(),
            truncated: false,
        };
        let actual = parse_arg(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_arg2() {
        let input = "\"foo\"...";
        let expected = TraceArg::String {
            value: "foo".to_string(),
            truncated: true,
        };
        let actual = parse_arg(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_arg3() {
        let input = "(BEANS)";
        let expected = vec![TraceArg::id("BEANS")];
        let actual = parse_args(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_is_ident_char() {
        assert!(is_ident_char('a'));
        assert!(is_ident_char('A'));
        assert!(is_ident_char('0'));
        assert!(is_ident_char('_'));
        assert!(!is_ident_char(' '));
        assert!(!is_ident_char('!'));
    }

    #[test]
    fn test_parse_integer() {
        let input = "(123)";
        let expected = vec![TraceArg::Integer {
            value: 123,
            annotation: None,
        }];
        let actual = parse_args(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_integer_with_annotation() {
        let input = "123</foo/bar/baz>";
        let expected = TraceArg::Integer {
            value: 123,
            annotation: Some(Annotation::File("/foo/bar/baz".to_string())),
        };
        let actual = parse_arg(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_arg_hex() {
        let input = "0x123";
        let expected = TraceArg::HexInteger {
            value: 0x123,
            comment: None,
        };
        let actual = parse_arg_hex(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_arg_hex_with_comment() {
        let input = "0x123 /* beans! */";
        let expected = TraceArg::HexInteger {
            value: 0x123,
            comment: Some("beans!".to_string()),
        };
        let actual = parse_arg(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_bitwise_or() {
        let input = "(foo | bar)";
        let expected = vec![TraceArg::BitwiseOr(
            Box::new(TraceArg::id("foo")),
            Box::new(TraceArg::id("bar")),
        )];
        let actual = parse_args(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_args() {
        let input = "(foo, bar, baz)";
        let expected = vec![
            TraceArg::id("foo"),
            TraceArg::id("bar"),
            TraceArg::id("baz"),
        ];
        let actual = parse_args(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_return() {
        let input = "-1 ENOMEM (Cannot allocate memory) <0.0>";
        let expected = TraceReturn::Normal {
            value: TraceArg::integer(-1),
            constant: Some("ENOMEM".to_string()),
            comment: Some("Cannot allocate memory".to_string()),
            duration: 0.0,
        };
        let actual = parse_return(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_f64() {
        let nums = vec!["-1.0", "-.0", ".0", "37.5"];
        for input in nums {
            let expected = input.parse::<f64>().unwrap();
            let actual = parse_float::<f64>(input).expect("parse failed").1;
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_parse_call() {
        let expected = Trace::Call {
            time: 1677286152.150443,
            name: "close".to_string(),
            args: vec![TraceArg::Integer {
                value: 2,
                annotation: Some(Annotation::CharDevice("/dev/pts/21".to_string(), 136, 21)),
            }],
            return_value: TraceReturn::Normal {
                value: TraceArg::integer(0),
                constant: None,
                comment: None,
                duration: 0.000074,
            },
        };
        let (_, actual) =
            parse_call("1677286152.150443 close(2</dev/pts/21<char 136:21>>) = 0 <0.000074>  ")
                .unwrap();
        assert_eq!(expected, actual);
    }

    // now to test (2</dev/pts/21<char 136:21>>)
    #[test]
    fn test_parse_args4() {
        let input = "2</dev/pts/21<char 136:21>>";
        let expected = TraceArg::Integer {
            value: 2,
            annotation: Some(Annotation::CharDevice("/dev/pts/21".to_string(), 136, 21)),
        };
        let actual = parse_arg(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_annotation() {
        // In some cases, an annotation can contain an annotation as in
        // </dev/pts/12<char 136:12>>
        // If an annotation references a file with < or > in it, those will be escaped as in
        // </home/dylan/Git/dylanwh/file-tracer/fish\74food\76>
        let input = "</dev/pts/21<char 136:21>>";
        let expected = Annotation::CharDevice("/dev/pts/21".to_string(), 136, 21);
        let actual = parse_annotation(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_annotation_2() {
        // In some cases, an annotation can contain an annotation as in
        // </dev/pts/12<char 136:12>>
        // If an annotation references a file with < or > in it, those will be escaped as in
        let input = r#"</home/dylan/Git/dylanwh/file-tracer/fish\74food\76>"#;
        let expected =
            Annotation::File("/home/dylan/Git/dylanwh/file-tracer/fish<food>".to_string());
        let actual = parse_annotation(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_array_1() {
        let input = "[...]";
        let expected = TraceArg::Array {
            values: vec![],
            truncated: true,
        };
        let actual = parse_arg_array(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }
    // ["foo", "bar"..., NULL, ...]
    #[test]
    fn test_parse_array_2() {
        let input = "[\"foo\", \"bar\"..., NULL, ...]";
        let expected = TraceArg::Array {
            values: vec![
                TraceArg::string("foo"),
                TraceArg::truncated_string("bar"),
                TraceArg::id("NULL"),
            ],
            truncated: true,
        };
        let actual = parse_arg_array(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_arg_5() {
        let input = r#"1677435419.657439 execve("/usr/bin/ls", ["ls", "-lh", "/home/dyla"..., ...], 0x7ffd7c50da00 /* 74 vars */) = 0 <0.000108>"#;
        let expected = Trace::Call {
            time: 1677435419.657439,
            name: "execve".into(),
            args: vec![
                TraceArg::string("/usr/bin/ls"),
                TraceArg::Array {
                    values: vec![
                        TraceArg::string("ls"),
                        TraceArg::string("-lh"),
                        TraceArg::truncated_string("/home/dyla"),
                    ],
                    truncated: true,
                },
                TraceArg::HexInteger {
                    value: 0x7ffd7c50da00,
                    comment: Some("74 vars".into()),
                },
            ],
            return_value: TraceReturn::Normal {
                value: TraceArg::integer(0),
                constant: None,
                comment: None,
                duration: 0.000108,
            },
        };
        let actual = parse_call(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_arg_oct() {
        let input = "(0777)";
        let expected = vec![TraceArg::OctInteger(0o777)];
        let actual = parse_args(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_args_or() {
        let input = r#"(0x7ff4f1a6a000, 106496, PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_FIXED)"#;
        let expected = vec![
            TraceArg::HexInteger {
                value: 0x7ff4f1a6a000,
                comment: None,
            },
            TraceArg::integer(106496),
            TraceArg::BitwiseOr(
                Box::new(TraceArg::id("PROT_READ")),
                Box::new(TraceArg::id("PROT_EXEC")),
            ),
            TraceArg::BitwiseOr(
                Box::new(TraceArg::id("MAP_PRIVATE")),
                Box::new(TraceArg::id("MAP_FIXED")),
            ),
        ];
        let actual = parse_args(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_arg_zero() {
        let input = "(0)";
        let expected = vec![TraceArg::integer(0)];
        let actual = parse_args(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_exit_group_0() {
        let input = r#"1677449784.694658 exit_group(0)         = ?"#;
        let expected = Trace::Call {
            time: 1677449784.694658,
            name: "exit_group".into(),
            args: vec![TraceArg::integer(0)],
            return_value: TraceReturn::Unknown,
        };
        let actual = parse_call(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_line_exit() {
        let input = "677449784.694738 +++ exited with 0 +++";
        let expected = Trace::Exit {
            time: 677449784.694738,
            status: 0,
        };
        let actual = parse_line(input).expect("parse failed").1;
        assert_eq!(expected, actual);
    }
}

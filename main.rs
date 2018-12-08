#![allow(dead_code)]

mod jass;

use crate::jass::lexer::Lexeme;
use crate::jass::lexer::Lexer;
use crate::jass::syntax::ProgramParser;
use codespan::Span;
use codespan::CodeMap;
use codespan_reporting::{Diagnostic, Label, emit};
use std::time::{Instant, Duration};
use lalrpop_util::ParseError;

fn lex_file(code_map: &mut CodeMap, file_name: &str) {
    let file_map = code_map.add_filemap_from_disk(file_name).unwrap();

    println!("[lexer] processing file {}", file_name);
    let lexer = Lexer::new(&file_map); //.filter_map(|s| s.ok().map(|s| s.inner));
    let timer_start = Instant::now();
    lexer.for_each(|lex| {
        if let Err(error) = lex {
            let error_span = error.span;
            let error = error.inner;

            let label = Label::new_primary(error_span).with_message("here");
            let diagnostic = Diagnostic::new_error(format!("{:?}", error)).with_label(label);
            emit(termcolor::StandardStream::stdout(termcolor::ColorChoice::Never), &code_map, &diagnostic).unwrap();
        }
    });
    println!("time elapsed: {:?}", timer_start.elapsed());
}

fn parse_file(code_map: &mut CodeMap, file_name: &str) {
    let file_map = code_map.add_filemap_from_disk(file_name).unwrap();

    println!("[parser] processing file {}", file_name);
    let lexer = Lexer::new(&file_map).filter_map(|lexeme| {
        lexeme.ok().map(|l| (l.span.start(), l.inner, l.span.end()))
    }).filter(|lexeme| {
        match lexeme {
            (_, Lexeme::LineComment(_), _) => false,
            (_, Lexeme::BlockComment(_), _) => false,
            _ => true,
        }
    });
    let timer_start = Instant::now();
    let program = ProgramParser::new().parse(file_map.src(), lexer);
    println!("time elapsed: {:?}", timer_start.elapsed());
    match program {
        Ok(program) => println!("{:#?}", program),
        Err(error) => {
            match error {
                ParseError::InvalidToken {location} => {
                    let span = Span::new(location, location);
                    let label = Label::new_primary(span).with_message("here");
                    let diagnostic = Diagnostic::new_error(format!("{:?}", error)).with_label(label);
                    emit(termcolor::StandardStream::stdout(termcolor::ColorChoice::Never), &code_map, &diagnostic).unwrap();
                },
                ParseError::UnrecognizedToken {token: Some((start, token, end)), expected} => {
                    let span = Span::new(start, end);
                    let label = Label::new_primary(span).with_message("here");
                    let expected = expected.iter().fold("".to_string(), |acc, x| acc + "," + x);
                    let diagnostic = Diagnostic::new_error(format!("unrecognized token {:?}, expected one of: {:?}", token, expected)).with_label(label);
                    emit(termcolor::StandardStream::stdout(termcolor::ColorChoice::Never), &code_map, &diagnostic).unwrap();
                },
                ParseError::UnrecognizedToken {token: None, expected} => {

                },
                ParseError::ExtraToken {..} => {

                }, 
                ParseError::User {..} => {

                }
            }
        }
    }
}

fn main() {
    let mut code_map = CodeMap::new();
    let file_names = vec!["blizzard.j", "common.j", "yarp.j"];
    file_names.iter().for_each(|file_name| {
        parse_file(&mut code_map, file_name);
    })
}
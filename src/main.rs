#![allow(dead_code)]

mod jass;

use crate::jass::lexer::Lexeme;
use crate::jass::lexer::Lexer;
use codespan::FileMap;
use codespan::CodeMap;
use codespan::FileName;
use codespan_reporting::{Diagnostic, Label, emit};
use std::time::{Instant, Duration};

fn process_file(code_map: &mut CodeMap, file_name: &str) {
    let file_map = code_map.add_filemap_from_disk(file_name).unwrap();

    println!("processing file {}", file_name);
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

fn main() {
    let mut code_map = CodeMap::new();
    let file_names = vec!["blizzard.j", "common.j", "yarp.j"];
    file_names.iter().for_each(|file_name| {
        process_file(&mut code_map, file_name);
    })
}
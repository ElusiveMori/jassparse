use codespan::*;
use std::str::Chars;
use std::str::CharIndices;

#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
pub struct Location {
    pub absolute: ByteIndex
}

impl Location {
    fn from_absolute(file_map: &FileMap, index: ByteIndex) -> Location {
        Location {
            absolute: index,
        }
    }

    fn from_location(file_map: &FileMap, line: LineIndex, column: ColumnIndex) -> Location {
        let index = file_map.byte_index(line, column).unwrap();

        Location {
            absolute: index,
        }
    }

    fn from_start(file_map: &FileMap) -> Location {
        let index = file_map.span().start();

        Self::from_absolute(file_map, index)
    }

    fn shift(mut self, ch: char) -> Location {
        self.absolute += ByteOffset(ch.len_utf8() as i64);
        self
    }
}

pub struct CharLocations<'input> {
    pub location: Location,
    chars: Chars<'input>,
}

impl<'input> CharLocations<'input> {
    pub fn new(input: &'input FileMap) -> CharLocations<'input> {
        CharLocations {
            location: Location::from_start(input),
            chars: input.src().chars(),
        }
    }
}

impl<'input> Iterator for CharLocations<'input> {
    type Item = (Location, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next().map(|ch| {
            let location = self.location;
            self.location = self.location.shift(ch);
            (location, ch)
        })
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Lexeme<'input> {
    LineComment(&'input str),
    BlockComment(&'input str),

    IntegerLiteral(i32),
    RealLiteral(i32),
    StringLiteral(&'input str),
    BooleanLiteral(bool),
    NullLiteral,

    Identifier(&'input str),

    // block delimeters
    For,
    EndFor,
    If,
    Then,
    Else,
    EndIf,
    Function,
    EndFunction,
    Globals,
    EndGlobals,
    Loop,
    EndLoop,

    // qualifiers
    Constant,
    Local,
    Native,
    Type,

    // control flow and signatures
    Takes,
    Returns,
    ExitWhen,
    Return,
    Set,
    Call,

    // * / + -
    Asterisk,
    Slash,
    Plus,
    Minus,

    // ( ) [ ]
    LParen,
    RParen,
    LBracket,
    RBracket,

    // built-in types
    Handle,
    Int,
    Bool,
    Real,
    Str,
    Code,
    Nothing,
}

pub enum LexicalError<'input> {
    Void(&'input str),
}

pub struct Spanned<T> {
    pub inner: T,
    pub span: Span<ByteIndex>,
}

fn spanned<T>(start: Location, end: Location, value: T) -> Spanned<T> {
    Spanned {
        inner: value,
        span: Span::new(start.absolute, end.absolute)
    }
}

pub type SpannedLexeme<'input> = Spanned<Lexeme<'input>>;
pub type SpannedError<'input> = Spanned<LexicalError<'input>>;

fn is_ident_start(ch: char) -> bool {
    match ch {
        '_' | 'a'...'z' | 'A'...'Z' => true,
        _ => false,
    }
}

fn is_ident_continue(ch: char) -> bool {
    match ch {
        '0'...'9' => true,
        ch => is_ident_start(ch),
    }
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

fn is_hex(ch: char) -> bool {
    ch.is_digit(16)
}

pub struct Lexer<'input> {
    file_map: &'input FileMap,
    input: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
    start_index: ByteIndex,
}

impl<'input> Lexer<'input> {
    fn new(file_map: &'input FileMap) -> Lexer<'input> {
        let input = file_map.src();
        let mut chars = input.char_indices();
        let lookahead = chars.next();

        Lexer {
            file_map,
            input,
            chars: chars,
            lookahead,
            start_index: file_map.span().start(),
        }
    }

    fn test_lookahead<F: FnMut(char) -> bool>(&self, mut predicate: F) -> bool {
        match self.lookahead {
            Some((index, ch)) => predicate(ch),
            None => false
        }
    }

    fn test_lookahead_char(&self, tester: char) -> bool {
        self.test_lookahead(|ch| ch == tester)
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        match self.lookahead {
            Some((index, ch)) => {
                self.lookahead = self.chars.next();
                Some((index, ch))
            },
            None => None
        }
    } 
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedLexeme<'input>, SpannedError<'input>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((index, ch)) = self.bump() {
            match ch {
                '/' if self.test_lookahead_char('/') => {
                    // line comment
                }
                '/' if self.test_lookahead_char('*') => {
                    // block comment
                }
                '/' => (),
                '*' => (),
                '+' => (),
                '-' => (),
                '0' if self.test_lookahead_char('x') => (),
                '$' => (),
                ch if is_ident_start(ch) => (),
                ch if is_digit(ch) || ch == '-' && self.test_lookahead_char('-') => (),
                ch if ch.is_whitespace() => continue,
                _ => unimplemented!()
            }
        }

        unimplemented!()
    }
}

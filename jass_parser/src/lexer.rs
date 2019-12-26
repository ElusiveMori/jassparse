use codespan::*;
use std::str::Chars;
use std::str::FromStr;

#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
pub struct Location {
    pub absolute: ByteIndex,
}

impl Location {
    fn from_absolute(index: ByteIndex) -> Location {
        Location { absolute: index }
    }

    fn from_location(file_map: &FileMap, line: LineIndex, column: ColumnIndex) -> Location {
        let index = file_map.byte_index(line, column).unwrap();

        Location { absolute: index }
    }

    fn from_start<S: AsRef<str>>(file_map: &FileMap<S>) -> Location {
        let index = file_map.span().start();

        Self::from_absolute(index)
    }

    fn shift(mut self, ch: char) -> Location {
        self.absolute += ByteOffset(ch.len_utf8() as i64);
        self
    }
}

struct CharLocations<'input> {
    pub location: Location,
    chars: Chars<'input>,
}

impl<'input> CharLocations<'input> {
    fn new<S: AsRef<str>>(input: &'input FileMap<S>) -> CharLocations<'input> {
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

    IntegerLiteral(u32),
    RealLiteral(f32),
    StringLiteral(&'input str),
    BooleanLiteral(bool),
    NullLiteral,

    Identifier(&'input str),

    // block delimeters
    If,
    Then,
    Else,
    ElseIf,
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
    Array,
    Extends,

    // control flow and signatures
    Takes,
    Returns,
    ExitWhen,
    Return,
    Set,
    Call,

    // * / + - ! > < =
    Asterisk,
    Slash,
    Plus,
    Minus,
    GreaterThan,
    LessThan,
    Assignment,

    // >= <= == !=
    GreaterThanEqual,
    LessThanEqual,
    Equal,
    NotEqual,

    // ( ) [ ] ,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,

    And,
    Or,
    Not,

    // built-in types
    Handle,
    Int,
    Bool,
    Real,
    Str,
    Code,
    Nothing,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LexicalError<'input> {
    UnexpectedEof(&'input str),
    UnexpectedEol(&'input str),
    UnexpectedChar(char),
    UnknownOperator(&'input str),
    MalformedLiteral(&'input str),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span<ByteIndex>,
}

pub fn spanned<T>(start: Location, end: Location, value: T) -> Spanned<T> {
    Spanned {
        inner: value,
        span: Span::new(start.absolute, end.absolute),
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
    ch.is_digit(10) || ch == '.'
}

fn is_hex(ch: char) -> bool {
    ch.is_digit(16)
}

fn is_operator(ch: char) -> bool {
    match ch {
        '+' => true,
        '-' => true,
        '*' => true,
        '/' => true,
        '[' => true,
        ']' => true,
        '(' => true,
        ')' => true,
        '>' => true,
        '<' => true,
        '=' => true,
        '!' => true,
        ',' => true,
        _ => false,
    }
}

pub struct Lexer<'input> {
    input: &'input str,
    chars: CharLocations<'input>,
    lookahead: Option<(Location, char)>,
    start_index: ByteIndex,
}

impl<'input> Lexer<'input> {
    pub fn new<S: AsRef<str>>(file_map: &'input FileMap<S>) -> Lexer<'input> {
        let input = file_map.src();
        let mut chars = CharLocations::new(file_map);
        let lookahead = chars.next();

        Lexer {
            input,
            chars: chars,
            lookahead,
            start_index: file_map.span().start(),
        }
    }

    fn test_lookahead<F: FnMut(char) -> bool>(&self, mut predicate: F) -> bool {
        match self.lookahead {
            Some((_, ch)) => predicate(ch),
            None => false,
        }
    }

    fn test_lookahead_char(&self, tester: char) -> bool {
        self.test_lookahead(|ch| ch == tester)
    }

    fn next_loc(&self) -> Location {
        self.lookahead.as_ref().map_or(self.chars.location, |l| l.0)
    }

    fn slice(&self, start: Location, end: Location) -> &'input str {
        let start = start.absolute - ByteOffset::from(self.start_index.to_usize() as i64);
        let end = end.absolute - ByteOffset::from(self.start_index.to_usize() as i64);

        &self.input[start.to_usize()..end.to_usize()]
    }

    fn bump(&mut self) -> Option<(Location, char)> {
        match self.lookahead {
            Some((index, ch)) => {
                self.lookahead = self.chars.next();
                Some((index, ch))
            }
            None => None,
        }
    }

    fn take_while<F>(&mut self, start: Location, mut keep_going: F) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |c| !keep_going(c))
    }

    fn take_until<F>(&mut self, start: Location, mut terminate: F) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead {
            if terminate(ch) {
                return (end, self.slice(start, end));
            } else {
                self.bump();
            }
        }
        (self.next_loc(), self.slice(start, self.next_loc()))
    }

    fn line_comment(&mut self, start: Location) -> SpannedLexeme<'input> {
        self.bump();

        while let Some((index, ch)) = self.bump() {
            if ch == '\n' {
                return spanned(start, index, Lexeme::LineComment(self.slice(start, index)));
            }
        }

        let end = self.chars.location;
        spanned(start, end, Lexeme::LineComment(self.slice(start, end)))
    }

    fn block_comment(
        &mut self,
        start: Location,
    ) -> Result<SpannedLexeme<'input>, SpannedError<'input>> {
        self.bump();

        while let Some((index, ch)) = self.bump() {
            if ch == '*' && self.test_lookahead_char('/') {
                return Ok(spanned(
                    start,
                    index,
                    Lexeme::BlockComment(self.slice(start, index)),
                ));
            }
        }

        let end = self.chars.location;
        Err(spanned(
            start,
            end,
            LexicalError::UnexpectedEof(self.slice(start, end)),
        ))
    }

    fn operator(
        &mut self,
        ch: char,
        start: Location,
    ) -> Result<SpannedLexeme<'input>, SpannedError<'input>> {
        let lexeme = match ch {
            '+' => Lexeme::Plus,
            '-' => Lexeme::Minus,
            '*' => Lexeme::Asterisk,
            '/' => Lexeme::Slash,
            '[' => Lexeme::LBracket,
            ']' => Lexeme::RBracket,
            '(' => Lexeme::LParen,
            ')' => Lexeme::RParen,
            '>' => {
                match self.lookahead {
                    Some((_, '=')) => {
                        self.bump();
                        Lexeme::GreaterThanEqual
                    },
                    _ => Lexeme::GreaterThan
                }
            },
            '<' => {
                match self.lookahead {
                    Some((_, '=')) => {
                        self.bump();
                        Lexeme::LessThanEqual
                    },
                    _ => Lexeme::LessThan
                }
            },
            '=' => {
                match self.lookahead {
                    Some((_, '=')) => {
                        self.bump();
                        Lexeme::Equal
                    },
                    _ => Lexeme::Assignment
                }
            },
            ',' => Lexeme::Comma,
            '!' => match self.bump() {
                Some((_, next)) => match next {
                    '=' => Lexeme::NotEqual,
                    _ => {
                        let end = self.next_loc();
                        return Err(spanned(
                            start,
                            end,
                            LexicalError::UnknownOperator(self.slice(start, end)),
                        ));
                    }
                },
                None => {
                    let end = self.next_loc();
                    return Err(spanned(
                        start,
                        end,
                        LexicalError::UnexpectedEof(self.slice(start, end)),
                    ));
                }
            },
            _ => unreachable!(),
        };

        Ok(spanned(start, self.next_loc(), lexeme))
    }

    fn hex_int(
        &mut self,
        mut start: Location,
    ) -> Result<SpannedLexeme<'input>, SpannedError<'input>> {
        if let Some((index, _)) = self.bump() {
            start = index;
        } else {
            return Err(spanned(
                start,
                start,
                LexicalError::UnexpectedEof(self.slice(start, start)),
            ));
        }

        loop {
            match self.lookahead {
                Some((_, ch)) if is_hex(ch) => {
                    self.bump();
                    continue;
                }
                Some((index, ch)) if is_ident_continue(ch) => {
                    let (end, _) = self.take_until(index, |ch| ch.is_whitespace());
                    return Err(spanned(
                        start,
                        end,
                        LexicalError::MalformedLiteral(self.slice(start, end)),
                    ));
                }
                _ => break,
            }
        }

        let end = self.next_loc();
        // let number: i32 = unsafe {
        //     std::mem::transmute(u32::from_str_radix(self.slice(start, end), 16).unwrap())
        // };
        let number = u32::from_str_radix(self.slice(start, end), 16).unwrap();
        self.bump();

        Ok(spanned(start, end, Lexeme::IntegerLiteral(number)))
    }

    fn dec_number(
        &mut self,
        start: Location,
        ch: char,
    ) -> Result<SpannedLexeme<'input>, SpannedError<'input>> {
        let mut is_real = ch == '.';

        loop {
            match self.lookahead {
                Some((index, ch)) if ch == '.' => {
                    if !is_real {
                        is_real = true;
                        self.bump();
                        continue;
                    } else {
                        let (end, _) = self.take_until(index, |ch| ch.is_whitespace());
                        return Err(spanned(
                            start,
                            end,
                            LexicalError::MalformedLiteral(self.slice(start, end)),
                        ));
                    }
                }
                Some((_, ch)) if is_digit(ch) => {
                    self.bump();
                    continue;
                }
                Some((index, ch)) if is_ident_continue(ch) => {
                    let (end, _) = self.take_until(index, |ch| ch.is_whitespace());
                    return Err(spanned(
                        start,
                        end,
                        LexicalError::MalformedLiteral(self.slice(start, end)),
                    ));
                }
                _ => break,
            }
        }

        let end = self.next_loc();
        let slice = self.slice(start, end);
        let lexeme = if !is_real {
            Lexeme::IntegerLiteral(u32::from_str(slice).unwrap())
        } else {
            Lexeme::RealLiteral(f32::from_str(slice).unwrap())
        };
        Ok(spanned(start, end, lexeme))
    }

    fn char_number(
        &mut self,
        start: Location,
    ) -> Result<SpannedLexeme<'input>, SpannedError<'input>> {
        use byteorder::BigEndian;
        use byteorder::ByteOrder;

        let (end, slice) = self.take_until(self.next_loc(), |ch| ch == '\'');
        self.bump();

        if slice.len() > 4 {
            return Err(spanned(start, end, LexicalError::MalformedLiteral(slice)));
        }

        let bytes = &slice.as_bytes();
        let num = BigEndian::read_u32(bytes);
        Ok(spanned(start, end, Lexeme::IntegerLiteral(num)))
    }

    fn string(&mut self, start: Location) -> Result<SpannedLexeme<'input>, SpannedError<'input>> {
        while let Some((end, ch)) = self.lookahead {
            match ch {
                '\\' => {
                    self.bump();
                }
                '\n' => {
                    self.bump();
                    return Err(spanned(
                        start,
                        end,
                        LexicalError::UnexpectedEol(self.slice(start, end)),
                    ));
                }
                '"' => {
                    self.bump();
                    return Ok(spanned(
                        start,
                        end,
                        Lexeme::StringLiteral(self.slice(start, end)),
                    ));
                }
                _ => {}
            }

            self.bump();
        }

        let end = self.next_loc();
        Err(spanned(
            start,
            end,
            LexicalError::UnexpectedEof(self.slice(start, end)),
        ))
    }

    fn identifier(&mut self, start: Location) -> SpannedLexeme<'input> {
        let (end, slice) = self.take_while(start, is_ident_continue);

        let lexeme = match slice {
            "true" => Lexeme::BooleanLiteral(true),
            "false" => Lexeme::BooleanLiteral(false),
            "null" => Lexeme::NullLiteral,

            "function" => Lexeme::Function,
            "endfunction" => Lexeme::EndFunction,
            "if" => Lexeme::If,
            "then" => Lexeme::Then,
            "else" => Lexeme::Else,
            "elseif" => Lexeme::ElseIf,
            "endif" => Lexeme::EndIf,
            "loop" => Lexeme::Loop,
            "endloop" => Lexeme::EndLoop,
            "globals" => Lexeme::Globals,
            "endglobals" => Lexeme::EndGlobals,

            "constant" => Lexeme::Constant,
            "array" => Lexeme::Array,
            "local" => Lexeme::Local,
            "native" => Lexeme::Native,
            "type" => Lexeme::Type,
            "extends" => Lexeme::Extends,

            "takes" => Lexeme::Takes,
            "return" => Lexeme::Return,
            "returns" => Lexeme::Returns,
            "exitwhen" => Lexeme::ExitWhen,
            "set" => Lexeme::Set,
            "call" => Lexeme::Call,

            "handle" => Lexeme::Handle,
            "integer" => Lexeme::Int,
            "real" => Lexeme::Real,
            "boolean" => Lexeme::Bool,
            "string" => Lexeme::Str,
            "code" => Lexeme::Code,
            "nothing" => Lexeme::Nothing,

            "and" => Lexeme::And,
            "or" => Lexeme::Or,
            "not" => Lexeme::Not,

            _ => Lexeme::Identifier(slice),
        };

        spanned(start, end, lexeme)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedLexeme<'input>, SpannedError<'input>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((index, ch)) = self.bump() {
            match ch {
                '/' if self.test_lookahead_char('/') => {
                    return Some(Ok(self.line_comment(index)));
                }
                '/' if self.test_lookahead_char('*') => {
                    return Some(self.block_comment(index));
                }
                '0' if self.test_lookahead_char('x') => {
                    if let Some((start, _)) = self.bump() {
                        return Some(self.hex_int(start));
                    }
                }
                '"' => return Some(self.string(index)),
                '\'' => return Some(self.char_number(index)),
                '$' => return Some(self.hex_int(index)),
                // ch if is_digit(ch) || (ch == '-' && self.test_lookahead(is_digit)) => {
                ch if is_digit(ch) => {
                    return Some(self.dec_number(index, ch))
                }
                ch if is_ident_start(ch) => return Some(Ok(self.identifier(index))),
                ch if is_operator(ch) => return Some(self.operator(ch, index)),
                ch if ch.is_whitespace() => continue,
                ch => {
                    return Some(Err(spanned(
                        index,
                        self.next_loc(),
                        LexicalError::UnexpectedChar(ch),
                    )))
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexeme;
    use crate::lexer::Lexer;
    use codespan::FileMap;
    use codespan::FileName;
    #[test]
    fn test_lexer_simple_function() {
        let file_map = FileMap::new(
            FileName::virtual_("test_file.j"),
            r#"
        function test_name takes integer a returns nothing
            call some_func(a)
            return
        endfunction
        "#,
        );

        let mut lexer = Lexer::new(&file_map).filter_map(|s| s.ok().map(|s| s.inner));

        assert_eq!(lexer.next().unwrap(), Lexeme::Function);
        assert_eq!(lexer.next().unwrap(), Lexeme::Identifier("test_name"));
        assert_eq!(lexer.next().unwrap(), Lexeme::Takes);
        assert_eq!(lexer.next().unwrap(), Lexeme::Int);
        assert_eq!(lexer.next().unwrap(), Lexeme::Identifier("a"));
        assert_eq!(lexer.next().unwrap(), Lexeme::Returns);
        assert_eq!(lexer.next().unwrap(), Lexeme::Nothing);
        assert_eq!(lexer.next().unwrap(), Lexeme::Call);
        assert_eq!(lexer.next().unwrap(), Lexeme::Identifier("some_func"));
        assert_eq!(lexer.next().unwrap(), Lexeme::LParen);
        assert_eq!(lexer.next().unwrap(), Lexeme::Identifier("a"));
        assert_eq!(lexer.next().unwrap(), Lexeme::RParen);
        assert_eq!(lexer.next().unwrap(), Lexeme::Return);
        assert_eq!(lexer.next().unwrap(), Lexeme::EndFunction);
    }
}

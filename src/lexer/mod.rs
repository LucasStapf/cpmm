#![allow(unused)]

use std::fmt::{Debug, Display, Formatter, write};
use crate::lexer::Keyword::To;

pub enum ValueType {
    Word,
    Number,
    Character,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenName {
    Identifier,
    Keyword(Keyword),
    Separator(Separator),
    Operator(Operator),
    Literal(Literal),
    Error(String),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub name: TokenName,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    fn new(name: TokenName, value: &'a str) -> Self {
        Self {
            name,
            value
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Keyword {
    Program,
    Begin,
    End,
    Const,
    Var,
    Procedure,
    Else,
    Read,
    Write,
    While,
    Do,
    If,
    Then,
    For,
    To,
    Integer,
    Real,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Separator {
    Comma,
    Dot,
    Semicolon,
    Colon,
    OpenParenthesis,
    CloseParenthesis,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqualTo,
    LessThanOrEqualTo,
    Assignment,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Literal {
    Integer,
    Real,
    InvalidFormat,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let n = self.name.to_string();
        write!(f, "Name: {:<30} Value: {}", n, self.value)
    }
}
//
// impl<'a> ToString for Token<'a> {
//     fn to_string(&self) -> String {
//         format!("Name: {} Value: {}", self.name, self.value)
//     }
// }
impl Display for TokenName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenName::Identifier => write!(f, "IDENTIFIER"),
            TokenName::Keyword(kw) => write!(f, "KEYWORD_{}", kw.to_string()),
            TokenName::Separator(sp) => write!(f, "SEPARATOR_{}", sp.to_string()),
            TokenName::Operator(op) => write!(f, "OPERATOR_{}", op.to_string()),
            TokenName::Literal(li) => write!(f, "LITERAL_{}", li.to_string()),
            TokenName::Error(str) => write!(f, "ERROR_{}", str),
            _ => write!(f, "Not implemented!"),
        }
    }
}

impl Keyword {
    fn is_keyword(v: &str) -> Option<Keyword> {
        match v {
            v if v == Keyword::Program.to_string() => Some(Keyword::Program),
            v if v == Keyword::Begin.to_string() => Some(Keyword::Begin),
            v if v == Keyword::End.to_string() => Some(Keyword::End),
            v if v == Keyword::Const.to_string() => Some(Keyword::Const),
            v if v == Keyword::Program.to_string() => Some(Keyword::Program),
            v if v == Keyword::Procedure.to_string() => Some(Keyword::Procedure),
            v if v == Keyword::Else.to_string() => Some(Keyword::Else),
            v if v == Keyword::Read.to_string() => Some(Keyword::Read),
            v if v == Keyword::Write.to_string() => Some(Keyword::Write),
            v if v == Keyword::While.to_string() => Some(Keyword::While),
            v if v == Keyword::Do.to_string() => Some(Keyword::Do),
            v if v == Keyword::If.to_string() => Some(Keyword::If),
            v if v == Keyword::Then.to_string() => Some(Keyword::Then),
            v if v == Keyword::For.to_string() => Some(Keyword::For),
            v if v == Keyword::To.to_string() => Some(Keyword::To),
            v if v == Keyword::Integer.to_string() => Some(Keyword::Integer),
            v if v == Keyword::Real.to_string() => Some(Keyword::Real),
            &_ => None
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Program => write!(f, "program"),
            Keyword::Begin => write!(f, "begin"),
            Keyword::End => write!(f, "end"),
            Keyword::Const => write!(f, "const"),
            Keyword::Program => write!(f, "program"),
            Keyword::Procedure => write!(f, "procedure"),
            Keyword::Else => write!(f, "else"),
            Keyword::Read => write!(f, "read"),
            Keyword::Write => write!(f, "write"),
            Keyword::While => write!(f, "while"),
            Keyword::Do => write!(f, "do"),
            Keyword::If => write!(f, "if"),
            Keyword::Then => write!(f, "then"),
            Keyword::For => write!(f, "for"),
            Keyword::To => write!(f, "to"),
            Keyword::Integer => write!(f, "integer"),
            Keyword::Real => write!(f, "real"),
            _ => write!(f, "Not implemented!"),
        }
    }
}

impl Separator {
    fn is_separator(c: &str) -> Option<Separator> {
        match c {
            c if c == "," => Some(Separator::Comma),
            c if c == "." => Some(Separator::Dot),
            c if c == ";" => Some(Separator::Semicolon),
            c if c == ":" => Some(Separator::Colon),
            c if c == "(" => Some(Separator::OpenParenthesis),
            c if c == ")" => Some(Separator::CloseParenthesis),
            _ => None,
        }
    }
}

impl Display for Separator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Separator::Comma => write!(f, "comma"),
            Separator::Dot => write!(f, "dot"),
            Separator::Semicolon => write!(f, "semicolon"),
            Separator::Colon => write!(f, "colon"),
            Separator::OpenParenthesis => write!(f, "open-parenthesis"),
            Separator::CloseParenthesis => write!(f, "close-parenthesis"),
            _ => write!(f, "Not implemented!"),
        }
    }
}

impl Operator {
    fn is_operator(o: &str) -> Option<Operator> {
        match o {
            o if o == "+" => Some(Operator::Addition),
            o if o == "-" => Some(Operator::Subtraction),
            o if o == "*" => Some(Operator::Multiplication),
            o if o == "/" => Some(Operator::Division),
            o if o == "=" => Some(Operator::Equal),
            o if o == "<>" => Some(Operator::NotEqual),
            o if o == ">" => Some(Operator::GreaterThan),
            o if o == "<" => Some(Operator::LessThan),
            o if o == ">=" => Some(Operator::GreaterThanOrEqualTo),
            o if o == "<=" => Some(Operator::LessThanOrEqualTo),
            o if o == ":=" => Some(Operator::Assignment),
            _ => None,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Addition => write!(f, "addition"),
            Operator::Subtraction => write!(f, "subtraction"),
            Operator::Multiplication => write!(f, "multiplication"),
            Operator::Division => write!(f, "division"),
            Operator::Equal => write!(f, "equal"),
            Operator::NotEqual => write!(f, "not-equal"),
            Operator::GreaterThan => write!(f, "greater-than"),
            Operator::LessThan => write!(f, "less-than"),
            Operator::GreaterThanOrEqualTo => write!(f, "greater-than-or-equal"),
            Operator::LessThanOrEqualTo => write!(f, "less-than-or-equal"),
            Operator::Assignment => write!(f, "assigment"),
            _ => write!(f, "Not implemented!"),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer => write!(f, "integer"),
            Literal::Real => write!(f, "real"),
            Literal::InvalidFormat => write!(f, "invalid_number_format"),
            _ => write!(f, "Not implemented!"),
        }
    }
}

pub fn next_token_value_type(chain: &str) -> ValueType {
    if chain.starts_with(char::is_alphabetic) {
        ValueType::Word
    } else if chain.starts_with(char::is_numeric) {
        ValueType::Number
    } else {
        ValueType::Character
    }
}

pub fn next_word(chain: &str) -> (&str, &str) {
    for (i, _c) in chain.match_indices(|c: char| c.is_whitespace() || c.is_ascii_punctuation() ) {
        return (&chain[..i], &chain[i..])
    }
    (&chain, "")
}


pub fn next_number(chain: &str) -> (Token, &str) {
    let mut i_dp = 0; // index of decimal pointer - if 0, there is no dp.
    for (i, c) in chain.char_indices() {
        if !c.is_numeric() {
            if i_dp != 0 {
                return if i_dp + 1 == i {
                    (Token::new(TokenName::Literal(Literal::InvalidFormat), &chain[0..i]), &chain[i..])
                } else {
                    (Token::new(TokenName::Literal(Literal::Real), &chain[0..i]), &chain[i..])
                }
            } else {
                if c == '.' {
                    i_dp = i;
                } else {
                    return (Token::new(TokenName::Literal(Literal::Integer), &chain[0..i]), &chain[i..]);
                }
            }
        }
    }

    if i_dp == 0 {
        (Token::new(TokenName::Literal(Literal::Integer), &chain), "")
    } else {
        if i_dp + 1 == chain.len() {
            (Token::new(TokenName::Literal(Literal::InvalidFormat), &chain), "")
        } else {
            (Token::new(TokenName::Literal(Literal::Real), &chain), "")
        }
    }
}

pub fn next_character(chain: &str) -> (Token, &str) {
    let mut chars = chain.chars();
    match chars.next().unwrap() {
        c if c == ',' => (Token::new(TokenName::Separator(Separator::Comma), &chain[0..1]), &chain[1..]),
        c if c == '.' => (Token::new(TokenName::Separator(Separator::Dot), &chain[0..1]), &chain[1..]),
        c if c == ';' => (Token::new(TokenName::Separator(Separator::Semicolon), &chain[0..1]), &chain[1..]),
        c if c == '(' => (Token::new(TokenName::Separator(Separator::OpenParenthesis), &chain[0..1]), &chain[1..]),
        c if c == ')' => (Token::new(TokenName::Separator(Separator::CloseParenthesis), &chain[0..1]), &chain[1..]),
        c if c == '+' => (Token::new(TokenName::Operator(Operator::Addition), &chain[0..1]), &chain[1..]),
        c if c == '-' => (Token::new(TokenName::Operator(Operator::Subtraction), &chain[0..1]), &chain[1..]),
        c if c == '*' => (Token::new(TokenName::Operator(Operator::Multiplication), &chain[0..1]), &chain[1..]),
        c if c == '/' => (Token::new(TokenName::Operator(Operator::Division), &chain[0..1]), &chain[1..]),
        c if c == '=' => (Token::new(TokenName::Operator(Operator::Equal), &chain[0..1]), &chain[1..]),
        c if c == '<' => {
            if let Some(c) = chars.next() {
                match c {
                    c if c == '>' => (Token::new(TokenName::Operator(Operator::NotEqual), &chain[0..2]), &chain[2..]),
                    c if c == '=' => (Token::new(TokenName::Operator(Operator::LessThanOrEqualTo), &chain[0..2]), &chain[2..]),
                    _ => (Token::new(TokenName::Operator(Operator::LessThan), &chain[0..1]), &chain[1..]),
                }
            } else {
                (Token::new(TokenName::Operator(Operator::LessThan), &chain[0..1]), &chain[1..])
            }
        }
        c if c == '>' => {
            if let Some(c) = chars.next() {
                match c {
                    c if c == '=' => (Token::new(TokenName::Operator(Operator::GreaterThanOrEqualTo), &chain[0..2]), &chain[2..]),
                    _ => (Token::new(TokenName::Operator(Operator::GreaterThan), &chain[0..1]), &chain[1..]),
                }
            } else {
                (Token::new(TokenName::Operator(Operator::GreaterThan), &chain[0..1]), &chain[1..])
            }
        }
        c if c == ':' => {
            if let Some(c) = chars.next() {
                match c {
                    c if c == '=' => (Token::new(TokenName::Operator(Operator::Assignment), &chain[0..2]), &chain[2..]),
                    _ => (Token::new(TokenName::Separator(Separator::Colon), &chain[0..1]), &chain[1..]),
                }
            } else {
                (Token::new(TokenName::Separator(Separator::Colon), &chain[0..1]), &chain[1..])
            }
        }
        _ => (Token::new(TokenName::Error("invalid_character".to_string()), &chain[0..1]), &chain[1..]),
    }
}

pub fn consume_comments_and_special_chars(chain: &str) -> &str {
    if chain.starts_with('{') {
        if let Some((_, s2)) = chain.split_once('}') {
            consume_comments_and_special_chars(s2.trim_start())
        } else {
            ""
        }
    } else {
        &chain
    }
}

pub fn next_token(chain: &str) -> (Token, &str) {
    let mut c = chain.trim_start();

    if c.starts_with(|c: char| c == '{' || c == '\n' || c == '\r') {
        c = consume_comments_and_special_chars(c);
    }

    match next_token_value_type(c) {
        ValueType::Word => {
            let (v, c) = next_word(c);
            if let Some(kw) = Keyword::is_keyword(v) {
                (Token::new(TokenName::Keyword(kw), v), c)
            } else {
                (Token::new(TokenName::Identifier, v), c)
            }
        },
        ValueType::Number => next_number(c),
        ValueType::Character => next_character(c),
        _ => panic!(""),
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Keyword, Literal, next_token, next_word, Operator, Token, TokenName, ValueType};

    #[test]
    fn single_word() {
        assert_eq!(next_word("program"), ("program", ""));
    }

    #[test]
    fn single_word_with_whitespace_at_the_end() {
        assert_eq!(next_word("program      "), ("program", "      "));
    }

    #[test]
    fn words_separated_by_comma() {
        assert_eq!(next_word("v1,v2,v3"), ("v1", ",v2,v3"));
    }

    #[test]
    fn words_separated_by_whitespaces() {
        assert_eq!(next_word("program testando123;"), ("program", " testando123;"));
    }

    #[test]
    fn next_keyword_token() {
        assert_eq!(next_token("program test123;"),
                   (Token::new(TokenName::Keyword(Keyword::Program), "program"), " test123;"));
    }

    #[test]
    fn last_word_token() {
        assert_eq!(next_token("id123"), (Token::new(TokenName::Identifier, "id123"), ""));
    }

    #[test]
    fn integer_literal_token() {
        assert_eq!(next_token("1023;"), (Token::new(TokenName::Literal(Literal::Integer), "1023"), ";"));
    }

    #[test]
    fn last_integer_literal_token() {
        assert_eq!(next_token("1023"), (Token::new(TokenName::Literal(Literal::Integer), "1023"), ""));
    }

    #[test]
    fn real_literal_token() {
        assert_eq!(next_token("24.16;"), (Token::new(TokenName::Literal(Literal::Real), "24.16"), ";"));
    }

    #[test]
    fn last_real_literal_token() {
        assert_eq!(next_token("24.16"), (Token::new(TokenName::Literal(Literal::Real), "24.16"), ""));
    }

    #[test]
    fn invalid_format_literal_token_1() {
        assert_eq!(next_token("24..16"), (Token::new(TokenName::Literal(Literal::InvalidFormat), "24."), ".16"));
    }

    #[test]
    fn invalid_format_literal_token_2() {
        assert_eq!(next_token("41."), (Token::new(TokenName::Literal(Literal::InvalidFormat), "41."), ""));
    }

    #[test]
    fn invalid_format_literal_token_3() {
        assert_eq!(next_token("103.@3"), (Token::new(TokenName::Literal(Literal::InvalidFormat), "103."), "@3"));
    }

    #[test]
    fn last_char_token() {
        assert_eq!(next_token("<>"), (Token::new(TokenName::Operator(Operator::NotEqual), "<>"), ""));
    }

    #[test]
    fn next_char_token_1() {
        assert_eq!(next_token(">= 4"), (Token::new(TokenName::Operator(Operator::GreaterThanOrEqualTo), ">="), " 4"));
    }

    #[test]
    fn next_char_token_2() {
        assert_eq!(next_token("=3"), (Token::new(TokenName::Operator(Operator::Equal), "="), "3"));
    }
}
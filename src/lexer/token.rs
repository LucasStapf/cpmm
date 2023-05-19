use std::fmt::{Debug, Display, Formatter, write};

pub enum ValueType {
    Word,
    Number,
    Character,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenName {
    Identifier,
    Keyword(KeywordType),
    Separator(SeparatorType),
    Operator(OperatorType),
    Literal(LiteralType),
    Error(String),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub name: TokenName,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    pub(crate) fn new(name: TokenName, value: &'a str) -> Self {
        Self {
            name,
            value
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum KeywordType {
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
pub enum SeparatorType {
    Comma,
    Dot,
    Semicolon,
    Colon,
    OpenParenthesis,
    CloseParenthesis,
}

#[derive(Debug, Eq, PartialEq)]
pub enum OperatorType {
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
pub enum LiteralType {
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

impl KeywordType {
    pub(crate) fn is_keyword(v: &str) -> Option<KeywordType> {
        match v {
            v if v == KeywordType::Program.to_string() => Some(KeywordType::Program),
            v if v == KeywordType::Begin.to_string() => Some(KeywordType::Begin),
            v if v == KeywordType::End.to_string() => Some(KeywordType::End),
            v if v == KeywordType::Const.to_string() => Some(KeywordType::Const),
            v if v == KeywordType::Program.to_string() => Some(KeywordType::Program),
            v if v == KeywordType::Procedure.to_string() => Some(KeywordType::Procedure),
            v if v == KeywordType::Else.to_string() => Some(KeywordType::Else),
            v if v == KeywordType::Read.to_string() => Some(KeywordType::Read),
            v if v == KeywordType::Write.to_string() => Some(KeywordType::Write),
            v if v == KeywordType::While.to_string() => Some(KeywordType::While),
            v if v == KeywordType::Do.to_string() => Some(KeywordType::Do),
            v if v == KeywordType::If.to_string() => Some(KeywordType::If),
            v if v == KeywordType::Then.to_string() => Some(KeywordType::Then),
            v if v == KeywordType::For.to_string() => Some(KeywordType::For),
            v if v == KeywordType::To.to_string() => Some(KeywordType::To),
            v if v == KeywordType::Integer.to_string() => Some(KeywordType::Integer),
            v if v == KeywordType::Real.to_string() => Some(KeywordType::Real),
            &_ => None
        }
    }
}

impl Display for KeywordType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KeywordType::Program => write!(f, "program"),
            KeywordType::Begin => write!(f, "begin"),
            KeywordType::End => write!(f, "end"),
            KeywordType::Const => write!(f, "const"),
            KeywordType::Program => write!(f, "program"),
            KeywordType::Procedure => write!(f, "procedure"),
            KeywordType::Else => write!(f, "else"),
            KeywordType::Read => write!(f, "read"),
            KeywordType::Write => write!(f, "write"),
            KeywordType::While => write!(f, "while"),
            KeywordType::Do => write!(f, "do"),
            KeywordType::If => write!(f, "if"),
            KeywordType::Then => write!(f, "then"),
            KeywordType::For => write!(f, "for"),
            KeywordType::To => write!(f, "to"),
            KeywordType::Integer => write!(f, "integer"),
            KeywordType::Real => write!(f, "real"),
            _ => write!(f, "Not implemented!"),
        }
    }
}

impl SeparatorType {
    pub fn is_separator(c: &str) -> Option<SeparatorType> {
        match c {
            c if c == "," => Some(SeparatorType::Comma),
            c if c == "." => Some(SeparatorType::Dot),
            c if c == ";" => Some(SeparatorType::Semicolon),
            c if c == ":" => Some(SeparatorType::Colon),
            c if c == "(" => Some(SeparatorType::OpenParenthesis),
            c if c == ")" => Some(SeparatorType::CloseParenthesis),
            _ => None,
        }
    }
}

impl Display for SeparatorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SeparatorType::Comma => write!(f, "comma"),
            SeparatorType::Dot => write!(f, "dot"),
            SeparatorType::Semicolon => write!(f, "semicolon"),
            SeparatorType::Colon => write!(f, "colon"),
            SeparatorType::OpenParenthesis => write!(f, "open-parenthesis"),
            SeparatorType::CloseParenthesis => write!(f, "close-parenthesis"),
            _ => write!(f, "Not implemented!"),
        }
    }
}

impl OperatorType {
    pub fn is_operator(o: &str) -> Option<OperatorType> {
        match o {
            o if o == "+" => Some(OperatorType::Addition),
            o if o == "-" => Some(OperatorType::Subtraction),
            o if o == "*" => Some(OperatorType::Multiplication),
            o if o == "/" => Some(OperatorType::Division),
            o if o == "=" => Some(OperatorType::Equal),
            o if o == "<>" => Some(OperatorType::NotEqual),
            o if o == ">" => Some(OperatorType::GreaterThan),
            o if o == "<" => Some(OperatorType::LessThan),
            o if o == ">=" => Some(OperatorType::GreaterThanOrEqualTo),
            o if o == "<=" => Some(OperatorType::LessThanOrEqualTo),
            o if o == ":=" => Some(OperatorType::Assignment),
            _ => None,
        }
    }
}

impl Display for OperatorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorType::Addition => write!(f, "addition"),
            OperatorType::Subtraction => write!(f, "subtraction"),
            OperatorType::Multiplication => write!(f, "multiplication"),
            OperatorType::Division => write!(f, "division"),
            OperatorType::Equal => write!(f, "equal"),
            OperatorType::NotEqual => write!(f, "not-equal"),
            OperatorType::GreaterThan => write!(f, "greater-than"),
            OperatorType::LessThan => write!(f, "less-than"),
            OperatorType::GreaterThanOrEqualTo => write!(f, "greater-than-or-equal"),
            OperatorType::LessThanOrEqualTo => write!(f, "less-than-or-equal"),
            OperatorType::Assignment => write!(f, "assigment"),
            _ => write!(f, "Not implemented!"),
        }
    }
}

impl Display for LiteralType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralType::Integer => write!(f, "integer"),
            LiteralType::Real => write!(f, "real"),
            LiteralType::InvalidFormat => write!(f, "invalid_number_format"),
            _ => write!(f, "Not implemented!"),
        }
    }
}
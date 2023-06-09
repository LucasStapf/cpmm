use crate::lexer::token::*;
use crate::lexer::OperatorType;
use crate::lexer::token::TokenName::{Invalid, Operator, Separator};

pub mod token;

pub struct Lexer<'a> {
    string: &'a str,
    line: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(string: &'a str) -> Self {
        Self {
            string,
            line: 1,
        }
    }

    fn consume_left_whitespaces(&mut self) {
        if self.string.starts_with(char::is_whitespace) {
            for (i, c) in self.string.char_indices() {
                match c {
                    c if c == ' ' || c == '\t' => continue,
                    c if c == '\r' || c == '\n' => self.line += 1,
                    _ => {
                        self.string = &self.string[i..];
                        break;
                    }
                }
            }
        }
    }

    fn consume_comments(&mut self) {
        if self.string.starts_with('{') {
            let mut flag = false;
            for (i, c) in self.string.char_indices() {
                if flag {
                    self.string = &self.string[i..];
                    break;
                }
                match c {
                    c if c == '\r' || c == '\n' => self.line += 1,
                    c if c == '}' => flag = true,
                    _ => continue,
                }
            }
        }
    }

    fn next_value_type(&self) -> ValueType {
        if self.string.starts_with(char::is_alphabetic) {
            ValueType::Word
        } else if self.string.starts_with(char::is_numeric) {
            ValueType::Number
        } else {
            ValueType::Character
        }
    }

    fn next_character(&mut self) -> Token {
        if self.string.len() >= 2 {
            if let Some(op) = OperatorType::is_operator(&self.string[..2]) {
                let token = Token::new(Operator(op), &self.string[..2]);
                self.string = &self.string[2..];
                return token
            }
        }

        return if let Some(op) = OperatorType::is_operator(&self.string[..1]) {
            let token = Token::new(Operator(op), &self.string[..1]);
            self.string = &self.string[1..];
            token
        } else if let Some(sp) = SeparatorType::is_separator(&self.string[..1]) {
            let token = Token::new(Separator(sp), &self.string[..1]);
            self.string = &self.string[1..];
            token
        } else {
            let token = Token::new(Invalid, &self.string[..1]);
            self.string = &self.string[1..];
            token
        }
    }

    fn next_number(&mut self) -> Token {
        let mut i_dp = 0; // index of decimal pointer - if 0, there is no dp.
        for (i, c) in self.string.char_indices() {
            if !c.is_numeric() {
                if i_dp != 0 {
                    return if i_dp + 1 == i {
                        let token = Token::new(Invalid, &self.string[0..i]);
                        self.string = &self.string[i..];
                        token
                    } else {
                        let token = Token::new(TokenName::Literal(LiteralType::Real), &self.string[0..i]);
                        self.string = &self.string[i..];
                        token
                    }
                } else {
                    if c == '.' {
                        i_dp = i;
                    } else {
                        let token = Token::new(TokenName::Literal(LiteralType::Integer), &self.string[0..i]);
                        self.string = &self.string[i..];
                        return token
                    }
                }
            }
        }

        if i_dp == 0 {
            Token::new(TokenName::Literal(LiteralType::Integer), &self.string)
        } else {
            if i_dp + 1 == self.string.len() {
                let token = Token::new(Invalid, &self.string);
                token
            } else {
                Token::new(TokenName::Literal(LiteralType::Real), &self.string)
            }
        }
    }

    fn next_word(&mut self) -> Token {
        let mut index = 0;
        for (i, _) in self.string.match_indices(|c: char| c.is_whitespace() || c.is_ascii_punctuation()) {
            index = i;
            break;
        }

        return if let Some(kw) = KeywordType::is_keyword(&self.string[..index]) {
            let token = Token::new(TokenName::Keyword(kw), &self.string[..index]);
            self.string = &self.string[index..];
            token
        } else {
            let token = Token::new(TokenName::Identifier, &self.string[..index]);
            self.string = &self.string[index..];
            token
        }
    }

    pub fn current_line(&self) -> u32 {
        self.line
    }

    pub fn string_is_empty(&self) -> bool {
        self.string.is_empty()
    }

    pub fn next_token(&mut self) -> Token {
        while self.string.starts_with('{') || self.string.starts_with(char::is_whitespace) {
            self.consume_left_whitespaces();
            self.consume_comments();
        }

        match self.next_value_type() {
            ValueType::Character => self.next_character(),
            ValueType::Number => self.next_number(),
            ValueType::Word => self.next_word(),
        }
    }
}
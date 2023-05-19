#![allow(unused)]

use crate::lexer::token::*;
use crate::lexer::OperatorType;
use crate::lexer::token::TokenName::{Identifier, Keyword, Operator, Separator};

mod token;

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

    fn next_character(&mut self) -> Result<Token, String> {
        if self.string.len() >= 2 {
            if let Some(op) = OperatorType::is_operator(&self.string[..2]) {
                let token = Token::new(Operator(op), &self.string[..2]);
                self.string = &self.string[2..];
                return Ok(token);
            }
        }

        return if let Some(op) = OperatorType::is_operator(&self.string[..1]) {
            let token = Token::new(Operator(op), &self.string[..1]);
            self.string = &self.string[1..];
            Ok(token)
        } else if let Some(sp) = SeparatorType::is_separator(&self.string[..1]) {
            let token = Token::new(Separator(sp), &self.string[..1]);
            self.string = &self.string[1..];
            Ok(token)
        } else {
            let ret = format!("Invalid character '{}'", &self.string[..1]);
            self.string = &self.string[1..];
            Err(ret)
        }
    }

    fn next_number(&mut self) -> Result<Token, String> {
        let mut i_dp = 0; // index of decimal pointer - if 0, there is no dp.
        for (i, c) in self.string.char_indices() {
            if !c.is_numeric() {
                if i_dp != 0 {
                    return if i_dp + 1 == i {
                        let ret = format!("Invalid format number {}", &self.string[0..i]);
                        self.string = &self.string[i..];
                        Err(ret)
                    } else {
                        let ret = Token::new(TokenName::Literal(LiteralType::Real), &self.string[0..i]);
                        self.string = &self.string[i..];
                        Ok(ret)
                    }
                } else {
                    if c == '.' {
                        i_dp = i;
                    } else {
                        let ret = Token::new(TokenName::Literal(LiteralType::Integer), &self.string[0..i]);
                        self.string = &self.string[i..];
                        return Ok(ret);
                    }
                }
            }
        }

        if i_dp == 0 {
            Ok(Token::new(TokenName::Literal(LiteralType::Integer), &self.string))
        } else {
            if i_dp + 1 == self.string.len() {
                let ret = format!("Invalid format number {}", &self.string);
                Err(ret)
            } else {
                Ok(Token::new(TokenName::Literal(LiteralType::Real), &self.string))
            }
        }
    }

    fn next_word(&mut self) -> Result<Token, String> {
        let mut index = 0;
        for (i, _) in self.string.match_indices(|c: char| c.is_whitespace() || c.is_ascii_punctuation()) {
            index = i;
            break;
        }

        return if let Some(kw) = KeywordType::is_keyword(&self.string[..index]) {
            let token = Token::new(TokenName::Keyword(kw), &self.string[..index]);
            self.string = &self.string[index..];
            Ok(token)
        } else {
            let token = Token::new(TokenName::Identifier, &self.string[..index]);
            self.string = &self.string[index..];
            Ok(token)
        }
    }

    pub fn current_line(&self) -> u32 {
        self.line
    }

    pub fn string_is_empty(&self) -> bool {
        self.string.is_empty()
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
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

// fn next_token_value_type(chain: &str) -> ValueType {
//     if chain.starts_with(char::is_alphabetic) {
//         ValueType::Word
//     } else if chain.starts_with(char::is_numeric) {
//         ValueType::Number
//     } else {
//         ValueType::Character
//     }
// }

// pub fn next_word(chain: &str) -> (&str, &str) {
//     for (i, _c) in chain.match_indices(|c: char| c.is_whitespace() || c.is_ascii_punctuation() ) {
//         return (&chain[..i], &chain[i..])
//     }
//     (&chain, "")
// }

// pub fn next_character(chain: &str) -> (Token, &str) {
//     let mut chars = chain.chars();
//     match chars.next().unwrap() {
//         c if c == ',' => (Token::new(TokenName::Separator(Separator::Comma), &chain[0..1]), &chain[1..]),
//         c if c == '.' => (Token::new(TokenName::Separator(Separator::Dot), &chain[0..1]), &chain[1..]),
//         c if c == ';' => (Token::new(TokenName::Separator(Separator::Semicolon), &chain[0..1]), &chain[1..]),
//         c if c == '(' => (Token::new(TokenName::Separator(Separator::OpenParenthesis), &chain[0..1]), &chain[1..]),
//         c if c == ')' => (Token::new(TokenName::Separator(Separator::CloseParenthesis), &chain[0..1]), &chain[1..]),
//         c if c == '+' => (Token::new(TokenName::Operator(Operator::Addition), &chain[0..1]), &chain[1..]),
//         c if c == '-' => (Token::new(TokenName::Operator(Operator::Subtraction), &chain[0..1]), &chain[1..]),
//         c if c == '*' => (Token::new(TokenName::Operator(Operator::Multiplication), &chain[0..1]), &chain[1..]),
//         c if c == '/' => (Token::new(TokenName::Operator(Operator::Division), &chain[0..1]), &chain[1..]),
//         c if c == '=' => (Token::new(TokenName::Operator(Operator::Equal), &chain[0..1]), &chain[1..]),
//         c if c == '<' => {
//             if let Some(c) = chars.next() {
//                 match c {
//                     c if c == '>' => (Token::new(TokenName::Operator(Operator::NotEqual), &chain[0..2]), &chain[2..]),
//                     c if c == '=' => (Token::new(TokenName::Operator(Operator::LessThanOrEqualTo), &chain[0..2]), &chain[2..]),
//                     _ => (Token::new(TokenName::Operator(Operator::LessThan), &chain[0..1]), &chain[1..]),
//                 }
//             } else {
//                 (Token::new(TokenName::Operator(Operator::LessThan), &chain[0..1]), &chain[1..])
//             }
//         }
//         c if c == '>' => {
//             if let Some(c) = chars.next() {
//                 match c {
//                     c if c == '=' => (Token::new(TokenName::Operator(Operator::GreaterThanOrEqualTo), &chain[0..2]), &chain[2..]),
//                     _ => (Token::new(TokenName::Operator(Operator::GreaterThan), &chain[0..1]), &chain[1..]),
//                 }
//             } else {
//                 (Token::new(TokenName::Operator(Operator::GreaterThan), &chain[0..1]), &chain[1..])
//             }
//         }
//         c if c == ':' => {
//             if let Some(c) = chars.next() {
//                 match c {
//                     c if c == '=' => (Token::new(TokenName::Operator(Operator::Assignment), &chain[0..2]), &chain[2..]),
//                     _ => (Token::new(TokenName::Separator(Separator::Colon), &chain[0..1]), &chain[1..]),
//                 }
//             } else {
//                 (Token::new(TokenName::Separator(Separator::Colon), &chain[0..1]), &chain[1..])
//             }
//         }
//         _ => (Token::new(TokenName::Error("invalid_character".to_string()), &chain[0..1]), &chain[1..]),
//     }
// }

// pub fn consume_comments_and_special_chars(chain: &str) -> &str {
//     if chain.starts_with('{') {
//         if let Some((_, s2)) = chain.split_once('}') {
//             consume_comments_and_special_chars(s2.trim_start())
//         } else {
//             ""
//         }
//     } else {
//         &chain
//     }
// }

// pub fn next_token(chain: &str) -> (Token, &str) {
//     let mut c = chain.trim_start();
//
//     if c.starts_with(|c: char| c == '{' || c == '\n' || c == '\r') {
//         c = consume_comments_and_special_chars(c);
//     }
//
//     match next_token_value_type(c) {
//         ValueType::Word => {
//             let (v, c) = next_word(c);
//             if let Some(kw) = Keyword::is_keyword(v) {
//                 (Token::new(TokenName::Keyword(kw), v), c)
//             } else {
//                 (Token::new(TokenName::Identifier, v), c)
//             }
//         },
//         ValueType::Number => next_number(c),
//         ValueType::Character => next_character(c),
//         _ => panic!(""),
//     }
// }

// #[cfg(test)]
// mod tests {
//     use crate::lexer::{KeywordType, LiteralType, next_token, next_word, Operator, Token, TokenName, ValueType};
//
//     #[test]
//     fn single_word() {
//         assert_eq!(next_word("program"), ("program", ""));
//     }
//
//     #[test]
//     fn single_word_with_whitespace_at_the_end() {
//         assert_eq!(next_word("program      "), ("program", "      "));
//     }
//
//     #[test]
//     fn words_separated_by_comma() {
//         assert_eq!(next_word("v1,v2,v3"), ("v1", ",v2,v3"));
//     }
//
//     #[test]
//     fn words_separated_by_whitespaces() {
//         assert_eq!(next_word("program testando123;"), ("program", " testando123;"));
//     }
//
//     #[test]
//     fn next_keyword_token() {
//         assert_eq!(next_token("program test123;"),
//                    (Token::new(TokenName::Keyword(KeywordType::Program), "program"), " test123;"));
//     }
//
//     #[test]
//     fn last_word_token() {
//         assert_eq!(next_token("id123"), (Token::new(TokenName::Identifier, "id123"), ""));
//     }
//
//     #[test]
//     fn integer_literal_token() {
//         assert_eq!(next_token("1023;"), (Token::new(TokenName::Literal(LiteralType::Integer), "1023"), ";"));
//     }
//
//     #[test]
//     fn last_integer_literal_token() {
//         assert_eq!(next_token("1023"), (Token::new(TokenName::Literal(LiteralType::Integer), "1023"), ""));
//     }
//
//     #[test]
//     fn real_literal_token() {
//         assert_eq!(next_token("24.16;"), (Token::new(TokenName::Literal(LiteralType::Real), "24.16"), ";"));
//     }
//
//     #[test]
//     fn last_real_literal_token() {
//         assert_eq!(next_token("24.16"), (Token::new(TokenName::Literal(LiteralType::Real), "24.16"), ""));
//     }
//
//     #[test]
//     fn invalid_format_literal_token_1() {
//         assert_eq!(next_token("24..16"), (Token::new(TokenName::Literal(LiteralType::InvalidFormat), "24."), ".16"));
//     }
//
//     #[test]
//     fn invalid_format_literal_token_2() {
//         assert_eq!(next_token("41."), (Token::new(TokenName::Literal(LiteralType::InvalidFormat), "41."), ""));
//     }
//
//     #[test]
//     fn invalid_format_literal_token_3() {
//         assert_eq!(next_token("103.@3"), (Token::new(TokenName::Literal(LiteralType::InvalidFormat), "103."), "@3"));
//     }
//
//     #[test]
//     fn last_char_token() {
//         assert_eq!(next_token("<>"), (Token::new(TokenName::Operator(Operator::NotEqual), "<>"), ""));
//     }
//
//     #[test]
//     fn next_char_token_1() {
//         assert_eq!(next_token(">= 4"), (Token::new(TokenName::Operator(Operator::GreaterThanOrEqualTo), ">="), " 4"));
//     }
//
//     #[test]
//     fn next_char_token_2() {
//         assert_eq!(next_token("=3"), (Token::new(TokenName::Operator(Operator::Equal), "="), "3"));
//     }
// }
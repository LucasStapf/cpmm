use std::env;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use crate::lexer::Lexer;

mod lexer;

fn main() -> std::io::Result<()> {

    let args: Vec<String> = env::args().collect();
    if let Some(f_in) = args.get(1) {
        if let Some(f_out) = args.get(2) {
            let mut fin = File::open(f_in)?;
            let mut fout = OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .open(f_out)?;

            let mut input = String::new();
            if let Ok(_) = fin.read_to_string(&mut input) {
                let string = input.as_str();
                let mut lex = Lexer::new(string);
                loop {
                    match lex.next_token() {
                        Ok(token) => write!(fout, "{}\n", token).expect("Could not write to file!"),
                        Err(msg) => write!(fout, "(At line {}) => ERROR: {}\n", lex.current_line(), msg).expect("Could not write to file!"),
                    }
                    if lex.string_is_empty() {
                        break;
                    }
                }
            }
        }
    }
    Ok(())
}

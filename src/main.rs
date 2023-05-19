use std::fs::{File, OpenOptions};
use std::io::{Read, Write};

mod lexer;

fn main() -> std::io::Result<()> {

    let mut fin = File::open("./input/teste_2.pmm")?;
    // let mut fout = File::create("./output/test_1_output")?;
    let mut fout = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open("./output/test_2_output")?;

    let mut input = String::new();
    if let Ok(_) = fin.read_to_string(&mut input) {
        let mut chain = input.as_str();
        while chain.len() > 0 {
            let (t, c) = lexer::next_token(chain);
            // println!("{}", t.to_string());
            write!(fout, "{}\n", t).expect("Could not write to file!");
            chain = c;
        }
    }

    Ok(())
}

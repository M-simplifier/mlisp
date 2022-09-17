use std::{io::stdin, iter::Peekable, num::ParseIntError, str::Chars};

#[derive(Debug)]
enum Token {
    OP,
    Num(i32),
    CP,
}

fn readi32(chars: &mut Peekable<Chars>) -> Result<i32, ParseIntError> {
    let mut digits = String::new();
    while match chars.peek() {
        None => false,
        Some(c) => c.is_numeric(),
    } {
        digits.push(chars.next().unwrap());
    }
    digits.parse::<i32>()
}

fn tokenize(target: &str) -> Result<Vec<Token>, &'static str> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut target = target.chars().peekable();
    loop {
        match target.peek() {
            None => break,
            Some('(') => {
                tokens.push(Token::OP);
                target.next();
                continue;
            }
            Some(')') => {
                tokens.push(Token::CP);
                target.next();
                continue;
            }
            Some(c) if c.is_whitespace() => {
                target.next();
                continue;
            }
            Some(c) if c.is_numeric() => {
                tokens.push(Token::Num(readi32(&mut target).unwrap()));
                continue;
            }
            _ => return Err("Cannot Tokenize"),
        }
    }
    Ok(tokens)
}

fn main() {
    let mut buf = String::new();
    if let Err(error) = stdin().read_line(&mut buf) {
        println!("{error}");
        return;
    }
    println!("{:?}", tokenize(buf.as_str()));
}

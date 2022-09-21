use std::{io::stdin, iter::Peekable, num::ParseIntError, slice::Iter, str::Chars};

trait S {
    fn evaluate(&self) -> Box<dyn S>;
    fn print(&self);
}

enum List {
    Cons { car: Box<dyn S>, cdr: Box<List> },
    Nil,
}

impl List {
    fn append(self, s: Box<dyn S>) -> List {
        match self {
            List::Nil => List::Cons {
                car: s,
                cdr: Box::new(List::Nil),
            },
            List::Cons { car, cdr } => List::Cons {
                car: car,
                cdr: Box::new(cdr.append(s)),
            },
        }
    }
}

impl S for List {
    fn evaluate(&self) -> Box<dyn S> {
        todo!()
    }

    fn print(&self) {
        match self {
            List::Nil => print!("()"),
            List::Cons { car, cdr } => {
                print!("(");
                car.print();
                let mut list = &**cdr;
                loop {
                    match list {
                        List::Nil => {
                            print!(")");
                            break;
                        }
                        List::Cons { car, cdr } => {
                            print!(" ");
                            car.print();
                            list = &**cdr;
                        }
                    }
                }
            }
        }
    }
}

struct I32(i32);

impl S for I32 {
    fn evaluate(&self) -> Box<dyn S> {
        let I32(v) = self;
        Box::new(I32(*v))
    }

    fn print(&self) {
        print!("{}", self.0);
    }
}

fn parse(tokens: &mut Peekable<Iter<Token>>) -> Result<Box<dyn S>, &'static str> {
    match tokens.peek() {
        None => Err("No tokens"),
        Some(Token::Num(v)) => Ok(Box::new(I32(*v))),
        Some(Token::OP) => {
            let mut list = List::Nil;
            loop {
                tokens.next();
                match tokens.peek() {
                    None => return Err("Missing close parenthesis"),
                    Some(Token::CP) => {
                        break;
                    }
                    Some(Token::Num(v)) => {
                        list = list.append(Box::new(I32(*v)));
                    }
                    Some(Token::OP) => {
                        let parsed = parse(tokens);
                        match parsed {
                            Err(error) => return Err(error),
                            Ok(s) => {
                                list = list.append(s);
                            }
                        }
                    }
                }
            }
            Ok(Box::new(list))
        }
        Some(Token::CP) => Err("Missing open parenthesis"),
    }
}

#[derive(Debug)]
enum Token {
    OP,
    Num(i32),
    CP,
}

fn read_i32(chars: &mut Peekable<Chars>) -> Result<i32, ParseIntError> {
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
                tokens.push(Token::Num(read_i32(&mut target).unwrap()));
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
    match tokenize(buf.as_str()) {
        Err(error) => println!("{error}"),
        Ok(tokens) => {
            let mut tokens = tokens.iter().peekable();
            match parse(&mut tokens) {
                Err(error) => println!("{error}"),
                Ok(s) => {
                    s.print();
                }
            }
        }
    }
}

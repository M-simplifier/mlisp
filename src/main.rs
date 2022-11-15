use std::{
    collections::HashMap, io::stdin, iter::Peekable, num::ParseIntError, slice::Iter, str::Chars,
};

type Context = HashMap<String, S>;

#[derive(Clone)]
enum S {
    Cons { car: Box<S>, cdr: Box<S> },
    Nil,
    I32(i32),
    Symbol(String),
    Add,
    Mul,
    Let,
    True,
    False,
    If,
    Lambda,
    Func { symbol: String, body: Box<S> },
    Greater,
}

impl S {
    fn freeze_context(&self, context: &Context) -> S {
        match self {
            S::Cons { car, cdr } => S::Cons {
                car: Box::new(car.freeze_context(context)),
                cdr: Box::new(cdr.freeze_context(context)),
            },
            S::Symbol(symbol) => match context.get(symbol) {
                Some(s) => s.clone(),
                _ => self.clone(),
            },
            other => other.clone(),
        }
    }
    fn apply(self, args: S, context: &Context) -> Result<S, &'static str> {
        match self {
            S::Add => {
                let mut sum = 0;
                let mut s = args;
                loop {
                    match s {
                        S::Nil => break,
                        S::Cons { car, cdr } => match car.evaluate(&context) {
                            Err(error) => return Err(error),
                            Ok(car) => match car {
                                S::I32(v) => {
                                    sum += v;
                                    s = *cdr;
                                }
                                _ => {
                                    return Err(
                                        "Add Error: non-numeric values cannot be multiplied.",
                                    )
                                }
                            },
                        },
                        _ => return Err("Add Error: arguments is not a list."),
                    }
                }
                Ok(S::I32(sum))
            }
            S::Mul => {
                let mut prod = 1;
                let mut s = args;
                loop {
                    match s {
                        S::Nil => break,
                        S::Cons { car, cdr } => match car.evaluate(&context) {
                            Err(error) => return Err(error),
                            Ok(car) => match car {
                                S::I32(v) => {
                                    prod *= v;
                                    s = *cdr;
                                }
                                _ => {
                                    return Err(
                                        "Mul Error: non-numeric values cannot be multiplied.",
                                    )
                                }
                            },
                        },
                        _ => return Err("Mul Error: arguments is not a list."),
                    }
                }
                Ok(S::I32(prod))
            }
            S::Greater => match args {
                S::Cons { car, cdr } => match car.evaluate(context) {
                    Ok(s) => match s {
                        S::I32(lval) => match *cdr {
                            S::Cons { car, cdr } => match car.evaluate(context) {
                                Ok(s) => match s {
                                    S::I32(rval) => match *cdr {
                                        S::Nil => {
                                            if lval < rval {
                                                Ok(S::True)
                                            } else {
                                                Ok(S::False)
                                            }
                                        }
                                        _ => {
                                            todo!("Error handling")
                                        }
                                    },
                                    _ => {
                                        todo!("Error handling")
                                    }
                                },
                                Err(error) => {
                                    todo!()
                                }
                            },
                            _ => {
                                todo!("Error handling")
                            }
                        },
                        _ => {
                            todo!("Error handling")
                        }
                    },
                    Err(error) => {
                        todo!()
                    }
                },
                _ => {
                    todo!("Error handling")
                }
            },
            S::Let => {
                let mut context = context.clone();
                match args {
                    S::Cons { car, cdr } => match *car {
                        S::Symbol(symbol) => match *cdr {
                            S::Cons { car, cdr } => match car.evaluate(&context) {
                                Err(error) => Err(error),
                                Ok(s) => {
                                    context.insert(symbol, s);
                                    match *cdr {
                                        S::Cons { car, cdr } => match *cdr {
                                            S::Nil => car.evaluate(&context),
                                            S::Cons { car: _, cdr: _ } => {
                                                Err("Let Error: too many arguments.")
                                            }
                                            _ => Err("Let Error: arguments is not a list."),
                                        },
                                        S::Nil => Err("Let Error: too few arguments."),
                                        _ => Err("Let Error: arguments is not a list."),
                                    }
                                }
                            },
                            S::Nil => Err("Let Error: too few arguments."),
                            _ => Err("Let Error: arguments is not a list."),
                        },
                        _ => Err("Let Error: first argument must be a symbol."),
                    },
                    _ => Err("Let Error: arguments is not a list."),
                }
            }
            S::If => match args {
                S::Cons { car, cdr } => match car.evaluate(&context) {
                    Err(error) => Err(error),
                    Ok(s) => match s {
                        S::True => match *cdr {
                            S::Cons { car, cdr } => match *cdr {
                                S::Cons { car: _, cdr } => match *cdr {
                                    S::Nil => car.evaluate(&context),
                                    S::Cons { car: _, cdr: _ } => {
                                        Err("If Error: too many arguments.")
                                    }
                                    _ => Err("If Error: arguments is not a list."),
                                },
                                S::Nil => Err("If Error: too few arguments."),
                                _ => Err("If Error: arguments is not a list."),
                            },
                            S::Nil => Err("If Error: too few arguments."),
                            _ => Err("If Error: arguments is not a list."),
                        },
                        S::False => match *cdr {
                            S::Cons { car: _, cdr } => match *cdr {
                                S::Cons { car, cdr } => match *cdr {
                                    S::Nil => car.evaluate(&context),
                                    S::Cons { car: _, cdr: _ } => {
                                        Err("If Error: too many arguments.")
                                    }
                                    _ => Err("If Error: arguments is not a list."),
                                },
                                S::Nil => Err("If Error: too few arguments."),
                                _ => Err("If Error: arguments is not a list."),
                            },
                            S::Nil => Err("If Error: too few arguments."),
                            _ => Err("If Error: arguments is not a list."),
                        },
                        _ => Err("If Error: first argument must be a boolean."),
                    },
                },
                S::Nil => Err("If Error: too few arguments."),
                _ => Err("If Error: arguments is not a list."),
            },
            S::Lambda => match args {
                S::Cons { car, cdr } => match *car {
                    S::Symbol(symbol) => match *cdr {
                        S::Cons { car, cdr } => match *cdr {
                            S::Nil => Ok(S::Func { symbol, body: car }),
                            S::Cons { car: _, cdr: _ } => Err("Lambda Error: too many arguments."),
                            _ => Err("Lambda Error: arguments is not a list."),
                        },
                        S::Nil => Err("Lambda Error: too few arguments."),
                        _ => Err("Lambda Error: arguments is not a list."),
                    },
                    _ => Err("Lambda Error: first argument must be a symbol."),
                },
                S::Nil => Err("Lambda Error: too few arguments."),
                _ => Err("Lambda Error: arguments is not a list."),
            },
            S::Func { symbol, body } => match args {
                S::Cons { car, cdr } => match *cdr {
                    S::Nil => {
                        let mut context = context.clone();
                        context.insert(symbol, *car);
                        body.evaluate(&context)
                    }
                    S::Cons { car: _, cdr: _ } => Err("Func Error: too many arguments."),
                    _ => Err("Func Error: arguments is not a list."),
                },
                S::Nil => Err("Func Error: too few arguments."),
                _ => Err("Func Error: arguments is not a list."),
            },
            _ => Err("Invalid apply: List's first element must be applyable."),
        }
    }
    fn evaluate(&self, context: &Context) -> Result<S, &'static str> {
        let s = self.freeze_context(context);
        match s {
            S::Cons { car, cdr } => match car.evaluate(context) {
                Err(error) => Err(error),
                Ok(s) => s.apply(*cdr.clone(), context),
            },
            S::Symbol(symbol) => match context.get(&symbol) {
                None => Err("Symbol does not exist."),
                Some(s) => Ok(s.clone()),
            },
            other => Ok(other.clone()),
        }
    }
    fn print(&self) {
        match self {
            S::Cons { car, cdr } => {
                print!("(");
                car.print();
                let mut list = (**cdr).clone();
                loop {
                    match list {
                        S::Nil => {
                            print!(")");
                            break;
                        }
                        S::Cons { car, cdr } => {
                            print!(" ");
                            car.print();
                            list = *cdr;
                        }
                        other => {
                            print!(" . ");
                            other.print();
                            break;
                        }
                    }
                }
            }
            S::Nil => print!("()"),
            S::I32(v) => print!("{v}"),
            S::Symbol(symbol) => print!("{symbol}"),
            S::Add => print!("ADD"),
            S::Mul => print!("MUL"),
            S::Let => print!("LET"),
            S::True => print!("TRUE"),
            S::False => print!("FALSE"),
            S::If => print!("IF"),
            S::Lambda => print!("LAMBDA"),
            S::Func { symbol, body } => {
                print!("λ {symbol} . ");
                body.print()
            }
            S::Greater => print!("<"),
        }
    }

    fn append(self, s: S) -> Result<S, &'static str> {
        match self {
            S::Cons { car, cdr } => {
                let car = car;
                let cdr = cdr.append(s);
                match cdr {
                    Ok(cdr) => Ok(S::Cons {
                        car,
                        cdr: Box::new(cdr),
                    }),
                    Err(error) => Err(error),
                }
            }
            S::Nil => Ok(S::Cons {
                car: Box::new(s),
                cdr: Box::new(S::Nil),
            }),
            _ => Err("Not a list"),
        }
    }
}

fn parse(tokens: &mut Peekable<Iter<Token>>) -> Result<S, &'static str> {
    match tokens.peek() {
        None => Err("No tokens"),
        Some(Token::Num(v)) => Ok(S::I32(*v)),
        Some(Token::Symbol(symbol)) => Ok(S::Symbol(symbol.clone())),
        Some(Token::OP) => {
            let mut list = S::Nil;
            loop {
                tokens.next();
                match tokens.peek() {
                    None => return Err("Missing close parenthesis"),
                    Some(Token::CP) => {
                        break;
                    }
                    Some(Token::Num(v)) => match list.append(S::I32(*v)) {
                        Ok(l) => list = l,
                        Err(error) => return Err(error),
                    },
                    Some(Token::Symbol(symbol)) => match list.append(S::Symbol(symbol.clone())) {
                        Ok(l) => list = l,
                        Err(error) => return Err(error),
                    },
                    Some(Token::OP) => {
                        let parsed = parse(tokens);
                        match parsed {
                            Err(error) => return Err(error),
                            Ok(s) => match list.append(s) {
                                Ok(l) => list = l,
                                Err(error) => return Err(error),
                            },
                        }
                    }
                }
            }
            Ok(list)
        }
        Some(Token::CP) => Err("Missing open parenthesis"),
    }
}

#[derive(Debug)]
enum Token {
    OP,
    Num(i32),
    Symbol(String),
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

fn read_symbol(chars: &mut Peekable<Chars>) -> String {
    let mut string = String::new();
    while match chars.peek() {
        None => false,
        Some(c) => !c.is_whitespace() && *c != '(' && *c != ')',
    } {
        string.push(chars.next().unwrap());
    }
    string
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
            Some(_) => {
                tokens.push(Token::Symbol(read_symbol(&mut target)));
                continue;
            }
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
                    let mut context = Context::new();
                    context.insert(String::from("+"), S::Add);
                    context.insert(String::from("*"), S::Mul);
                    context.insert(String::from("let"), S::Let);
                    context.insert(String::from("true"), S::True);
                    context.insert(String::from("false"), S::False);
                    context.insert(String::from("if"), S::If);
                    context.insert(String::from("lambda"), S::Lambda);
                    context.insert(String::from("<"), S::Greater);
                    match s.evaluate(&context) {
                        Err(error) => println!("{error}"),
                        Ok(s) => s.print(),
                    }
                }
            }
        }
    }
}

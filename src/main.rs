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
    List,
    Car,
    Add,
    Mul,
    Let,
    True,
    False,
    If,
    Lambda,
    Func { symbol: String, body: Box<S> },
    Greater,
    Defmacro,
    Macro { arglist: Box<S>, body: Box<S> },
    // Commands
    Define,
    DefineCommand { symbol: String, expr: Box<S> },
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
    fn car(&self) -> Result<S, &'static str> {
        match self {
            S::Cons { car, cdr: _ } => Ok(*car.clone()),
            _ => Err("Not cons"),
        }
    }
    fn cdr(&self) -> Result<S, &'static str> {
        match self {
            S::Cons { car: _, cdr } => Ok(*cdr.clone()),
            _ => Err("Not cons"),
        }
    }
    fn first(&self) -> Result<S, &'static str> {
        self.car()
    }
    fn second(&self) -> Result<S, &'static str> {
        self.cdr()?.car()
    }
    fn third(&self) -> Result<S, &'static str> {
        self.cdr()?.cdr()?.car()
    }
    fn length(&self) -> Result<i32, &'static str> {
        let mut length = 0;
        let mut list = self.clone();
        loop {
            match list {
                S::Nil => break,
                S::Cons { car: _, cdr } => {
                    length += 1;
                    list = *cdr.clone();
                }
                _ => length = -1,
            }
        }
        if length < 0 {
            Err("Not a list")
        } else {
            Ok(length)
        }
    }
    fn as_i32(&self) -> Result<i32, &'static str> {
        match self {
            S::I32(value) => Ok(*value),
            _ => Err("Not i32"),
        }
    }
    fn as_symbol(&self) -> Result<String, &'static str> {
        match self {
            S::Symbol(value) => Ok(value.clone()),
            _ => Err("Not symbol"),
        }
    }
    fn as_bool(&self) -> Result<bool, &'static str> {
        match self {
            S::True => Ok(true),
            S::False => Ok(false),
            _ => Err("Not bool"),
        }
    }
    fn apply(self, args: S, context: &Context) -> Result<S, &'static str> {
        match self {
            S::List => Ok(args),
            S::Car => {
                let list = args.car()?;
                list.evaluate(context)?.car()
            }
            S::Add => {
                let mut sum = 0;
                let mut s = args;
                loop {
                    match s {
                        S::Nil => break,
                        S::Cons { car, cdr } => {
                            let value = car.evaluate(&context)?.as_i32()?;
                            sum += value;
                            s = *cdr;
                        }
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
                        S::Cons { car, cdr } => {
                            let value = car.evaluate(&context)?.as_i32()?;
                            prod *= value;
                            s = *cdr;
                        }
                        _ => return Err("Mul Error: arguments is not a list."),
                    }
                }
                Ok(S::I32(prod))
            }
            // (< lhs:to-i32 rhs:to-i32)
            S::Greater => {
                let lhs = args.first()?.evaluate(&context)?.as_i32()?;
                let rhs = args.second()?.evaluate(&context)?.as_i32()?;

                if lhs < rhs {
                    Ok(S::True)
                } else {
                    Ok(S::False)
                }
            }
            // (let ident:symbol value:any body:any )
            S::Let => {
                let mut context = context.clone();
                let ident = args.first()?.as_symbol()?;
                let value = args.second()?.evaluate(&context)?;
                context.insert(ident, value);
                let body = args.third()?;
                body.evaluate(&context)
            }
            // (if con:to-bool then:any els:any)
            S::If => {
                let con = args.first()?.evaluate(context)?.as_bool()?;
                let then = args.second()?;
                let els = args.third()?;

                if con {
                    then.evaluate(context)
                } else {
                    els.evaluate(context)
                }
            }
            // (lambda arg:symbol body:any)
            S::Lambda => {
                let arg = args.first()?.as_symbol()?;
                let body = args.second()?;
                Ok(S::Func {
                    symbol: arg,
                    body: Box::new(body),
                })
            }
            // (function arg:any)
            S::Func { symbol, body } => {
                let arg = args.first()?.evaluate(context)?;
                let mut context = context.clone();
                context.insert(symbol, arg);
                body.evaluate(&context)
            }
            // (! arglist:list body:any)
            S::Defmacro => {
                let arglist = args.first()?;
                let body = args.second()?.freeze_context(context);

                let mcr = S::Macro {
                    arglist: Box::new(arglist),
                    body: Box::new(body),
                };
                Ok(mcr)
            }
            // (macro arg1 arg2 ..)
            S::Macro { arglist, body } => {
                let mut context = context.clone();
                if args.length()? != arglist.length()? {
                    return Err("invalid arguments");
                }
                let mut args = args;
                let mut arglist = *arglist;
                loop {
                    match args {
                        S::Nil => break,
                        S::Cons { car, cdr } => {
                            let arg = *car;
                            let symbol = arglist.first()?.as_symbol()?;
                            context.insert(symbol, arg);
                            args = *cdr;
                            arglist = arglist.cdr()?;
                        }
                        _ => break,
                    }
                }
                body.evaluate(&context)
            }
            // (define ident expr)
            S::Define => {
                let ident = args.first()?.as_symbol()?;
                let expr = args.second()?;
                let define_command = S::DefineCommand {
                    symbol: ident,
                    expr: Box::new(expr),
                };
                Ok(define_command)
            }
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
            S::List => print!("'"),
            S::Car => print!("CAR"),
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
            S::Defmacro => print!("!"),
            S::Macro {
                arglist: args,
                body,
            } => {
                print!("! ");
                args.print();
                print!(" => ");
                body.print()
            }
            S::Define => print!("DEFINE"),
            S::DefineCommand { symbol, expr } => {
                print!("define ident: {symbol} expr: ");
                expr.print();
            }
        }
    }

    fn append(self, s: S) -> Result<S, &'static str> {
        match self {
            S::Cons { car, cdr } => {
                let cdr = cdr.append(s)?;
                Ok(S::Cons {
                    car,
                    cdr: Box::new(cdr),
                })
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
                    Some(Token::CP) => break,
                    Some(Token::Num(v)) => list = list.append(S::I32(*v))?,
                    Some(Token::Symbol(symbol)) => list = list.append(S::Symbol(symbol.clone()))?,
                    Some(Token::OP) => {
                        let parsed = parse(tokens)?;
                        list = list.append(parsed)?;
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
    let mut context = Context::new();
    context.insert(String::from("'"), S::List);
    context.insert(String::from("car"), S::Car);
    context.insert(String::from("+"), S::Add);
    context.insert(String::from("*"), S::Mul);
    context.insert(String::from("let"), S::Let);
    context.insert(String::from("true"), S::True);
    context.insert(String::from("false"), S::False);
    context.insert(String::from("if"), S::If);
    context.insert(String::from("lambda"), S::Lambda);
    context.insert(String::from("<"), S::Greater);
    context.insert(String::from("!"), S::Defmacro);
    context.insert(String::from("define"), S::Define);
    loop {
        let mut buf = String::new();
        if let Err(error) = stdin().read_line(&mut buf) {
            println!("{error}");
            return;
        }
        let tokens = match tokenize(buf.as_str()) {
            Ok(tokens) => tokens,
            Err(err) => {
                println!("{err}");
                continue;
            }
        };
        let mut tokens = tokens.iter().peekable();
        let s = match parse(&mut tokens) {
            Ok(s) => s,
            Err(err) => {
                println!("{err}");
                continue;
            }
        };

        match s.evaluate(&context) {
            Err(error) => println!("{error}"),
            Ok(S::DefineCommand { symbol, expr }) => {
                context.insert(symbol, *expr);
            }
            Ok(s) => {
                s.print();
                println!("")
            }
        }
    }
}

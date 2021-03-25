#![feature(type_name_of_val)]
#![feature(or_patterns)]
use std::any::type_name_of_val;

use logos::{Lexer, Logos};

#[derive(Logos, Debug, Copy, Clone, PartialEq)]
enum Token {
    #[regex(r"[a-zA-Z_][\w_]*")]
    Ident,

    #[regex(r"\d+")]
    #[regex(r"\d+[uU]")]
    #[regex(r"\d+[lL]")]
    #[regex(r"\d+[uU][lL]")]
    #[regex(r"\d+[uU][lL][lL]")]
    Num,

    #[regex(r"\d+\.\d*f?")]
    Real,

    #[regex(r"[\[\{\(]")]
    LDelim,
    #[regex(r"[\]\}\)]")]
    RDelim,

    #[token("->")]
    Access,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token(">>")]
    Shr,
    #[token("<<")]
    Shl,
    #[token("==")]
    Eq,
    #[token("!=")]
    Ne,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,

    #[token("++")]
    Inc,
    #[token("--")]
    Dec,

    #[regex(r"[~!&|^=?.,;:*/%+-]", |it| it.slice().as_bytes()[0] as char)]
    Punct(char),

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

impl Token {
    fn op(self) -> Binop {
        use Token::*;
        match self {
            Access => Binop::PtrField,
            Punct('.') => Binop::Field,
            Punct('*') => Binop::Mul,
            Punct('/') => Binop::Div,
            Punct('%') => Binop::Rem,
            Punct('+') => Binop::Add,
            Punct('-') => Binop::Sub,
            Shr => Binop::Shl,
            Shr => Binop::Shr,
            Le => Binop::Le,
            Ge => Binop::Ge,
            Punct('<') => Binop::Lt,
            Punct('>') => Binop::Gt,
            Eq => Binop::Eq,
            Ne => Binop::Ne,
            Punct('&') => Binop::And,
            Punct('^') => Binop::Xor,
            Punct('|') => Binop::Or,
            And => Binop::Logand,
            Or => Binop::Logor,
            _ => unreachable!(),
        }
    }

    fn prec(self) -> u32 {
        use Token::*;
        match self {
            Access | Punct('.') => 11,
            Punct('*' | '/' | '%') => 10,
            Punct('+' | '-') => 9,
            Shr | Shl => 8,
            Le | Ge | Punct('>' | '<') => 7,
            Eq | Ne => 6,
            Punct('&') => 5,
            Punct('^') => 4,
            Punct('|') => 3,
            And => 2,
            Or => 1,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Unop {
    Neg,
    Not,
    Flip,
    PostInc,
    PostDec,
    PreInc,
    PreDec,
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Field,
    PtrField,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Xor,
    Or,
    Shr,
    Shl,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    Logand,
    Logor,
    Subs,
}

#[derive(Debug, Clone, Copy)]
pub enum Suffix {
    U,
    UL,
    ULL,
    I,
    L,
    LL,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    Num(u64, Suffix),
    Real(f64),
    Unary(Unop, Box<Expr>),
    Binop(Binop, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Uint(u64),
    Sint(i64),
    Real(f64),
    Bool(bool),
}

impl Expr {
    pub fn new(s: &str) -> Option<Expr> {
        parse_expr(&mut Token::lexer(s)).ok()
    }
}

fn check_postfix(it: &mut Lexer<Token>, x: Expr) -> Result<Expr, ()> {
    use Token::*;
    Ok(match it.clone().next() {
        Some(Punct('[')) => {
            it.next();
            let y = parse_expr(it)?;
            match it.next() {
                Some(Punct(']')) => Expr::Binop(Binop::Subs, Box::new(x), Box::new(y)),
                _ => return Err(()),
            }
        }
        Some(Inc) => Expr::Unary(Unop::PostInc, Box::new(x)),
        Some(Dec) => Expr::Unary(Unop::PostDec, Box::new(x)),
        _ => x,
    })
}

fn parse_term(it: &mut Lexer<Token>) -> Result<Expr, ()> {
    use Token::*;
    let x = match it.next().ok_or(())? {
        Ident => Expr::Var(it.slice().to_owned()),
        Num => {
            let s: &str = it.slice();
            let mut cc = s.len();
            let mut v = String::with_capacity(4);
            while !(s.as_bytes()[cc - 1] as char).is_digit(10) {
                v.push(s.as_bytes()[cc - 1].to_ascii_lowercase() as char);
                cc -= 1;
            }
            let ss = match v.as_str() {
                "llu" => Suffix::ULL,
                "ll" => Suffix::LL,
                "lu" => Suffix::UL,
                "l" => Suffix::L,
                "u" => Suffix::U,
                _ => Suffix::I,
            };
            Expr::Num(it.slice()[..cc].parse().unwrap(), ss)
        }
        Real => Expr::Real(it.slice().replace("f", "").parse().unwrap()),
        LDelim => {
            let x = parse_expr(it)?;
            match it.next() {
                Some(RDelim) => x,
                _ => return Err(()),
            }
        }
        Punct('~') => Expr::Unary(Unop::Flip, Box::new(parse_term(it)?)),
        Punct('!') => Expr::Unary(Unop::Not, Box::new(parse_term(it)?)),
        Punct('-') => Expr::Unary(Unop::Neg, Box::new(parse_term(it)?)),
        Inc => Expr::Unary(Unop::PreInc, Box::new(parse_term(it)?)),
        Dec => Expr::Unary(Unop::PreDec, Box::new(parse_term(it)?)),
        _ => return Err(()),
    };
    check_postfix(it, x)
}

/*
expr = precn
...
prec1 = prec0 | prec1 + prec0
prec0 = term | prec0 * term
term = num | ( expr )
*/

fn parse_prec(it: &mut Lexer<Token>, mut lhs: Expr, prec: u32) -> Result<Expr, ()> {
    while it.clone().next().map(|t| t.prec() >= prec) == Some(true) {
        let t: Token = it.next().unwrap();
        let prec2 = t.prec();
        let mut rhs = parse_term(it)?;
        while it.clone().next().map(|t| t.prec() >= prec2) == Some(true) {
            rhs = parse_prec(it, rhs, prec + 1)?;
        }
        lhs = Expr::Binop(t.op(), Box::new(lhs), Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_expr(it: &mut Lexer<Token>) -> Result<Expr, ()> {
    let x = parse_term(it)?;
    parse_prec(it, x, 1)
}

#[test]
fn tests() {
    Expr::new("(rasterizationSamples + 31) / 32").unwrap();
    Expr::new("9 / 2 + a-b").unwrap();
    Expr::new("abc->abc + abc.abc - x12 * 12").unwrap();
    Expr::new("(~0U-2)").unwrap();
    println!("{:?}", Expr::new("(~0U-2)").unwrap());
}

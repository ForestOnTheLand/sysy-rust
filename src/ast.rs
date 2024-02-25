//! In this file, different kinds of AST are defined.

/// `CompUnit ::= FuncDef`
#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

/// `FuncDef ::= FuncType IDENT "(" ")" Block`
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

/// `FuncType ::= "int"`
#[derive(Debug)]
pub enum FuncType {
    Int,
}

/// `Block ::= "{" Stmt "}"`
#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

/// `Stmt ::= "return" Exp ";"`
#[derive(Debug)]
pub struct Stmt {
    pub exp: Box<Exp>,
}

/// Exp ::= UnaryExp;
#[derive(Debug)]
pub struct Exp {
    pub add_exp: Box<AddExp>,
}

/// PrimaryExp ::= "(" Exp ")" | Number;
#[derive(Debug)]
pub enum PrimaryExp {
    Expression(Box<Exp>),
    Number(i32),
}

/// UnaryExp ::= PrimaryExp | UnaryOp UnaryExp;
#[derive(Debug)]
pub enum UnaryExp {
    Single(Box<PrimaryExp>),
    Unary(UnaryOp, Box<UnaryExp>),
}

/// UnaryOp ::= "+" | "-" | "!";
#[derive(Debug)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

/// MulExp ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
#[derive(Debug)]
pub enum MulExp {
    Single(Box<UnaryExp>),
    Binary(Box<MulExp>, MulOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

/// AddExp ::= MulExp | AddExp ("+" | "-") MulExp;
#[derive(Debug)]
pub enum AddExp {
    Single(Box<MulExp>),
    Binary(Box<AddExp>, AddOp, Box<MulExp>),
}

#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
}

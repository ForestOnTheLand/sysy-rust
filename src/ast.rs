//! In this file, different kinds of AST are defined
//! using BNF, with the following special notations:
//!
//! - {A} means repeating A 0 or more times
//! - \[A\] means optional A
//!

/// [`CompUnit`] `::=` [`FuncDef`]
#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

/// [`FuncDef`] `::=` [`FuncType`] `IDENT` `"("` `")"` [`Block`]
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

/// [`FuncType`] `::=` `"int"`
#[derive(Debug)]
pub enum FuncType {
    Int,
}

/// [`Block`] `::=` `"{"` {[`BlockItem`]} `"}"`
#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<Box<BlockItem>>,
}

/// [`BlockItem`] `::=` [`Decl`] | [`Stmt`]
#[derive(Debug)]
pub enum BlockItem {
    Decl(Box<Decl>),
    Stmt(Box<Stmt>),
}

/// [`Stmt`] `::=` [`LVal`] `"="` [`Exp`] `";"`
///        | [`Exp`] `";"`
///        | [`Block`]
///        | `"return"` [`Exp`] `";"`
#[derive(Debug)]
pub enum Stmt {
    Assign(LVal, Box<Exp>),
    Exp(Option<Box<Exp>>),
    Block(Box<Block>),
    Return(Box<Exp>),
}

/// [`Decl`] `::=` [`ConstDecl`] | [`VarDecl`]
#[derive(Debug)]
pub enum Decl {
    Const(Box<ConstDecl>),
    Var(Box<VarDecl>),
}

/// [`VarDecl`] `::=` [`BType`] [`VarDef`] {`","` [`VarDef`]} `""`
#[derive(Debug)]
pub struct VarDecl {
    pub btype: BType,
    pub var_defs: Vec<Box<VarDef>>,
}

/// [`VarDef`] `::=` `IDENT` | `IDENT` `"="` [`InitVal`]
#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub init_val: Option<Box<InitVal>>,
}
/// [`InitVal`] `::=` [`Exp`]
#[derive(Debug)]
pub struct InitVal {
    pub exp: Box<Exp>,
}

/// [`ConstDecl`] `::=` `"const"` [`BType`] [`ConstDef`] {`","` [`ConstDef`]} ``";"``
#[derive(Debug)]
pub struct ConstDecl {
    pub btype: BType,
    pub const_defs: Vec<Box<ConstDef>>,
}

/// [`BType`] `::=` `"int"`
#[derive(Debug)]
pub enum BType {
    Int,
}

/// [`ConstDef`] `::=` `IDENT` `"="` [`ConstInitVal`]
#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: Box<ConstInitVal>,
}

/// [`ConstInitVal`] `::=` [`ConstExp`]
#[derive(Debug)]
pub struct ConstInitVal {
    pub const_exp: Box<ConstExp>,
}

/// [`LVal`] `::=` `IDENT`
#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

/// [`ConstExp`] `::=` [`Exp`]
#[derive(Debug)]
pub struct ConstExp {
    pub exp: Box<Exp>,
}

/// [`Exp`] `::=` [`UnaryExp`]
#[derive(Debug)]
pub struct Exp {
    pub lor_exp: Box<LOrExp>,
}

/// [`PrimaryExp`] `::=` `"("` [`Exp`] `")"` | [`LVal`] | `Number`
#[derive(Debug)]
pub enum PrimaryExp {
    Expression(Box<Exp>),
    LVal(LVal),
    Number(i32),
}

/// [`UnaryExp`] `::=` [`PrimaryExp`] | [`UnaryOp`] [`UnaryExp`]
#[derive(Debug)]
pub enum UnaryExp {
    Single(Box<PrimaryExp>),
    Unary(UnaryOp, Box<UnaryExp>),
}

/// [`UnaryOp`] `::=` `"+"` | `"-"` | `"!"`
#[derive(Debug)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

/// [`MulExp`] `::=` [`UnaryExp`] | [`MulExp`] (`"*"` | `"/"` | `"%"`) [`UnaryExp`]
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

/// [`AddExp`] `::=` [`MulExp`] | [`AddExp`] (`"+"` | `"-"`) [`MulExp`]
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

/// [`RelExp`] `::=` [`AddExp`] | [`RelExp`] (`"<"` | `">"` | `"<="` | `">="`) [`AddExp`]
#[derive(Debug)]
pub enum RelExp {
    Single(Box<AddExp>),
    Binary(Box<RelExp>, RelOp, Box<AddExp>),
}

#[derive(Debug)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

/// [`EqExp`] `::=` [`RelExp`] | [`EqExp`] (`"=="` | `"!="`) [`RelExp`]
#[derive(Debug)]
pub enum EqExp {
    Single(Box<RelExp>),
    Binary(Box<EqExp>, EqOp, Box<RelExp>),
}

#[derive(Debug)]
pub enum EqOp {
    Eq,
    Neq,
}

/// [`LAndExp`] `::=` [`EqExp`] | [`LAndExp`] `"&&"` [`EqExp`]
#[derive(Debug)]
pub enum LAndExp {
    Single(Box<EqExp>),
    Binary(Box<LAndExp>, Box<EqExp>),
}

/// [`LOrExp`] `::=` [`LAndExp`] | [`LOrExp`] `"||"` [`LAndExp`]
#[derive(Debug)]
pub enum LOrExp {
    Single(Box<LAndExp>),
    Binary(Box<LOrExp>, Box<LAndExp>),
}

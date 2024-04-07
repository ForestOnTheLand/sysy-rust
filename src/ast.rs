//! In this file, different kinds of AST are defined
//! using BNF, with the following special notations:
//!
//! - {`A`} means repeating `A` for 0 or more times
//! - \[`A`\] means optional `A`
//!
//! Some grammars are different from that in the writeup,
//! in order to eliminate ambiguity and conflict.
//!

/// [`BuiltinType`] `::=` `"void"` | `"int"`
#[derive(Debug, PartialEq, Eq)]
pub enum BuiltinType {
    Void,
    Int,
}

/// [`CompUnit`] `::=` {[`GlobalItem`]}
#[derive(Debug)]
pub struct CompUnit {
    pub items: Vec<Box<GlobalItem>>,
}

/// [`GlobalItem`] `::=` [`GlobalDecl`] | [`FuncDef`]
#[derive(Debug)]
pub enum GlobalItem {
    Decl(Box<GlobalDecl>),
    FuncDef(Box<FuncDef>),
}

/// [`GlobalDecl`] `::=` [`ConstDecl`] | [`GlobalVarDecl`]
#[derive(Debug)]
pub enum GlobalDecl {
    Const(Box<ConstDecl>),
    Var(Box<GlobalVarDecl>),
}

/// [`GlobalVarDecl`] `::=` [`BuiltinType`] [`GlobalVarDef`] {`","` [`GlobalVarDef`]} `";"`
#[derive(Debug)]
pub struct GlobalVarDecl {
    pub btype: BuiltinType,
    pub var_defs: Vec<Box<GlobalVarDef>>,
}

/// [`GlobalVarDef`] `::=` `IDENT` {`"["` [`ConstExp`] `"]"`}
///                     | `IDENT` {`"["` [`ConstExp`] `"]"`} `"="` [`ConstInitVal`]
#[derive(Debug)]
pub struct GlobalVarDef {
    pub ident: String,
    pub shape: Vec<Box<ConstExp>>,
    pub init_val: Option<Box<ConstInitVal>>,
}

/// [`FuncDef`] `::=` [`BuiltinType`] `IDENT` `"("` [[`FuncFParams`]] `")"` [`Block`]
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: BuiltinType,
    pub ident: String,
    pub params: Option<Box<FuncFParams>>,
    pub block: Block,
}

/// [`FuncFParams`] ::= [`FuncFParam`] {`","` [`FuncFParam`]}
#[derive(Debug)]
pub struct FuncFParams {
    pub params: Vec<Box<FuncFParam>>,
}

/// [`FuncFParam`]  ::= [`BuiltinType`] `IDENT` ["[" "]" {"[" ConstExp "]"}]
#[derive(Debug)]
pub struct FuncFParam {
    pub btype: BuiltinType,
    pub ident: String,
    pub shape: Option<Vec<Box<ConstExp>>>,
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
///        | `"if"` `"("` [`Exp`] `")"` [`Stmt`] [`"else"` [`Stmt`]]
///        | "while" "(" [`Exp`] ")" [`Stmt`]
///        | "break" ";"
///        | "continue" ";"
///        | `"return"` [[`Exp`]] `";"`
#[derive(Debug)]
pub enum Stmt {
    Assign(LVal, Box<Exp>),
    Exp(Option<Box<Exp>>),
    Block(Box<Block>),
    Condition(Box<Exp>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Exp>, Box<Stmt>),
    Break,
    Continue,
    Return(Option<Box<Exp>>),
}

/// [`Decl`] `::=` [`ConstDecl`] | [`VarDecl`]
#[derive(Debug)]
pub enum Decl {
    Const(Box<ConstDecl>),
    Var(Box<VarDecl>),
}

/// [`VarDecl`] `::=` [`BuiltinType`] [`VarDef`] {`","` [`VarDef`]} `""`
#[derive(Debug)]
pub struct VarDecl {
    pub btype: BuiltinType,
    pub var_defs: Vec<Box<VarDef>>,
}

/// [`VarDef`] `::=` `IDENT` {`"["` [`ConstExp`] `"]"`}
///               | `IDENT` {`"["` [`ConstExp`] `"]"`} `"="` [`InitVal`]
#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub shape: Vec<Box<ConstExp>>,
    pub init_val: Option<Box<InitVal>>,
}

/// [`InitVal`] `::=` [`Exp`] | `"{"` [[`InitVal`] {`","` [`InitVal`]}] `"}"`
#[derive(Debug)]
pub enum InitVal {
    Single(Box<Exp>),
    Array(Vec<Box<InitVal>>),
}

/// [`ConstDecl`] `::=` `"const"` [`BuiltinType`] [`ConstDef`] {`","` [`ConstDef`]} ``";"``
#[derive(Debug)]
pub struct ConstDecl {
    pub btype: BuiltinType,
    pub const_defs: Vec<Box<ConstDef>>,
}

// #[derive(Debug)]
// pub enum BuiltinType {
//     Int,
// }

/// [`ConstDef`] `::=` `IDENT` {`"["` [`ConstExp`] `"]"`} `"="` [`ConstInitVal`]
#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub shape: Vec<Box<ConstExp>>,
    pub const_init_val: Box<ConstInitVal>,
}

/// [`ConstInitVal`] `::=` [`ConstExp`] | "{" [[`ConstExp`] {"," [`ConstExp`]}] "}"
#[derive(Debug)]
pub enum ConstInitVal {
    Single(Box<ConstExp>),
    Array(Vec<Box<ConstInitVal>>),
}

/// [`LVal`] `::=` `IDENT` {`"["` [`Exp`] `"]"`}
#[derive(Debug)]
pub struct LVal {
    pub ident: String,
    pub index: Vec<Box<Exp>>,
}

/// [`ConstExp`] `::=` [`Exp`]
#[derive(Debug)]
pub struct ConstExp {
    pub exp: Box<Exp>,
}

/// [`Exp`] `::=` [`UnaryExp`]
#[derive(Debug)]
pub enum Exp {
    LVal(LVal),
    Number(i32),
    Binary(Box<Exp>, BinaryOperator, Box<Exp>),
    Unary(UnaryOperator, Box<Exp>),
    Call(String, Option<Box<FuncRParams>>),
}

/// [`FuncRParams`] ::= [`Exp`] {"," [`Exp`]};
#[derive(Debug)]
pub struct FuncRParams {
    pub exps: Vec<Box<Exp>>,
}

/// [`UnaryOperator`] `::=` `"+"` | `"-"` | `"!"`
#[derive(Debug)]
pub enum UnaryOperator {
    Pos,
    Neg,
    Not,
}

impl UnaryOperator {
    pub fn compute(&self, operand: i32) -> i32 {
        match self {
            UnaryOperator::Pos => operand,
            UnaryOperator::Neg => -operand,
            UnaryOperator::Not => (operand == 0) as i32,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    NotEq,
    Eq,
    Gt,
    Lt,
    Ge,
    Le,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Sar,
}

impl BinaryOperator {
    pub fn compute(&self, left: i32, right: i32) -> i32 {
        match self {
            BinaryOperator::NotEq => (left != right) as i32,
            BinaryOperator::Eq => (left == right) as i32,
            BinaryOperator::Gt => (left > right) as i32,
            BinaryOperator::Lt => (left < right) as i32,
            BinaryOperator::Ge => (left >= right) as i32,
            BinaryOperator::Le => (left <= right) as i32,
            BinaryOperator::Add => left + right,
            BinaryOperator::Sub => left - right,
            BinaryOperator::Mul => left * right,
            BinaryOperator::Div => left / right,
            BinaryOperator::Mod => left % right,
            BinaryOperator::And => (left != 0 && right != 0) as i32,
            BinaryOperator::Or => (left != 0 || right != 0) as i32,
            BinaryOperator::Xor => left ^ right,
            BinaryOperator::Shl => unimplemented!(),
            BinaryOperator::Shr => unimplemented!(),
            BinaryOperator::Sar => unimplemented!(),
        }
    }
}

impl From<BinaryOperator> for koopa::ir::BinaryOp {
    fn from(value: BinaryOperator) -> Self {
        use koopa::ir::BinaryOp;
        match value {
            BinaryOperator::NotEq => BinaryOp::NotEq,
            BinaryOperator::Eq => BinaryOp::Eq,
            BinaryOperator::Gt => BinaryOp::Gt,
            BinaryOperator::Lt => BinaryOp::Lt,
            BinaryOperator::Ge => BinaryOp::Ge,
            BinaryOperator::Le => BinaryOp::Le,
            BinaryOperator::Add => BinaryOp::Add,
            BinaryOperator::Sub => BinaryOp::Sub,
            BinaryOperator::Mul => BinaryOp::Mul,
            BinaryOperator::Div => BinaryOp::Div,
            BinaryOperator::Mod => BinaryOp::Mod,
            BinaryOperator::And => BinaryOp::And,
            BinaryOperator::Or => BinaryOp::Or,
            BinaryOperator::Xor => BinaryOp::Xor,
            BinaryOperator::Shl => BinaryOp::Shl,
            BinaryOperator::Shr => BinaryOp::Shr,
            BinaryOperator::Sar => BinaryOp::Sar,
        }
    }
}

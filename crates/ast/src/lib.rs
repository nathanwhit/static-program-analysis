extern crate self as ast;

pub use print::Print;

use core::fmt;

use common::{derive_common, Foldable, SuperFoldable};
pub mod fold;
pub mod print;
pub mod visit;

pub type ExpBox = Box<Exp>;

#[derive(Copy)]
#[derive_common]
pub struct Span(pub usize, pub usize);

#[derive(Copy)]
#[derive_common]
/// Binary operator
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Eq,
    NEq,
    Lt,
}

#[derive_common]
pub struct Id(pub String);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for Id {
    fn from(s: String) -> Self {
        Id(s)
    }
}

impl From<&str> for Id {
    fn from(s: &str) -> Self {
        Id(s.to_string())
    }
}

#[derive_common]
#[derive(SuperFoldable)]
/// Expression
pub enum Exp {
    /// Integer
    Int(#[fold(identity)] i64),
    /// Identifier
    Id(Id),
    /// (Exp)
    Paren(ExpBox),
    /// Exp Op Exp
    BinOp(ExpBox, #[fold(identity)] Op, ExpBox),
    /// input
    Input,
    /// fun(Exp, Exp, ...)
    Call(ExpBox, Vec<Exp>),
    /// alloc Exp
    Alloc(ExpBox),
    /// &Id
    Ref(Id),
    /// *Exp
    Deref(ExpBox),
    /// { Id: Exp, ..., Id: Exp }
    Record(Record),
    /// Exp.Id
    FieldAccess(ExpBox, Id),
    /// null
    Null,
    /// Error
    Error,
}

impl From<i64> for Exp {
    fn from(i: i64) -> Self {
        Exp::Int(i)
    }
}

impl From<Id> for Exp {
    fn from(id: Id) -> Self {
        Exp::Id(id)
    }
}

impl From<&str> for Exp {
    fn from(s: &str) -> Self {
        Exp::Id(Id::from(s))
    }
}

pub type StmBox = Box<Stm>;

#[derive_common]
#[derive(SuperFoldable)]
/// Statement
pub enum Stm {
    /// Id = Exp;
    Assign(Id, Exp),
    /// output Exp;
    Output(Exp),
    /// Stm*
    Block(Vec<Stm>),
    /// if Exp { Stm } else { Stm }
    If(Exp, StmBox, Option<StmBox>),
    /// while Exp { Stm }
    While(Exp, StmBox),
    /// var Id (, Id)*;
    Locals(Vec<Id>),
    /// return Exp;
    Return(Exp),
    /// *Exp = Exp;
    Store(Exp, Exp),
    /// Id.Id = Exp;
    FieldUpdate(Id, Id, Exp),
    /// (*Exp).Id = Exp;
    IndirectFieldUpdate(Exp, Id, Exp),
}

#[derive_common]
#[derive(SuperFoldable)]
/// Function
///
/// name(arg, arg, arg) { locals? stm return exp }
pub struct Fun {
    pub name: Id,
    pub args: Vec<Id>,
    pub locals: Vec<Id>,
    pub body: Vec<Stm>,
    pub ret: Exp,
}

#[derive_common]
#[derive(SuperFoldable)]
pub struct Program {
    pub funs: Vec<Fun>,
}

#[derive_common]
#[derive(Foldable)]
pub struct Record {
    pub fields: Vec<(Id, Exp)>,
}

impl From<Vec<(Id, Exp)>> for Record {
    fn from(fields: Vec<(Id, Exp)>) -> Self {
        Record { fields }
    }
}

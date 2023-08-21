extern crate self as ast;

pub use print::Print;

use core::fmt;
use std::fmt::Debug;

use common::{derive_common, la_arena, DebugWithCtx, Foldable, Get, SuperFoldable};
pub mod fold;
pub mod print;
pub mod visit;

pub type ExpBox = Box<Exp>;

macro_rules! newtype_id {
    (pub type $name: ident = $(common::)?Idx<$t: ty> in $arena: ident) => {
        #[derive_common]
        #[derive(Copy)]
        pub struct $name(::common::Idx<$t>);

        impl From<$name> for ::common::Idx<$t> {
            fn from(id: $name) -> Self {
                id.0
            }
        }

        impl From<$name> for ::common::la_arena::Idx<$t> {
            fn from(id: $name) -> Self {
                id.0.into()
            }
        }

        impl ::common::Lookup for $name {
            type Ctx = AstCtx;
            type Output = $t;

            fn lookup<'ctx>(&self, ctx: &'ctx Self::Ctx) -> &'ctx Self::Output {
                ctx.get(*self)
            }
        }

        impl ::common::Get<$name> for AstCtx {
            fn get(&self, idx: $name) -> &<$name as ::common::Lookup>::Output {
                &self.$arena[::common::la_arena::Idx::from(idx.0)]
            }

            fn get_mut(&mut self, idx: $name) -> &mut <$name as ::common::Lookup>::Output {
                &mut self.$arena[::common::la_arena::Idx::from(idx.0)]
            }
        }

        impl ::common::Lookup for &$name {
            type Ctx = AstCtx;
            type Output = $t;

            fn lookup<'ctx>(&self, ctx: &'ctx Self::Ctx) -> &'ctx Self::Output {
                ctx.get(*self)
            }
        }

        impl ::common::Get<&$name> for AstCtx {
            fn get(&self, idx: &$name) -> &<$name as ::common::Lookup>::Output {
                &self.$arena[::common::la_arena::Idx::from(idx.0)]
            }

            fn get_mut(&mut self, idx: &$name) -> &mut <$name as ::common::Lookup>::Output {
                &mut self.$arena[::common::la_arena::Idx::from(idx.0)]
            }
        }

        impl ::common::DebugWithCtx for $name {
            type Ctx = AstCtx;
            fn fmt_with_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Self::Ctx) -> fmt::Result {
                <_ as ::std::fmt::Debug>::fmt(&ctx.get(*self), f)
            }
        }

        #[cfg(any(test, feature = "serde"))]
        impl ::common::serde::SerializeWithCtx for $name {
            type Ctx = AstCtx;
            fn serialize_with_ctx<S>(
                &self,
                serializer: S,
                ctx: &Self::Ctx,
            ) -> Result<S::Ok, S::Error>
            where
                S: ::serde::Serializer,
            {
                <_ as ::common::serde::SerializeWithCtx>::serialize_with_ctx(
                    ctx.get(*self),
                    serializer,
                    ctx,
                )
            }
        }

        impl ::common::Allocable for $t {
            type Idx = $name;

            fn alloc_in<A: ::common::Alloc<Self>>(self, arena: &mut A) -> $name {
                arena.alloc(self)
            }
        }

        impl ::common::Alloc<$t> for AstCtx {
            fn alloc(&mut self, t: $t) -> $name {
                $name(::common::Idx::from(self.$arena.alloc(t)))
            }
        }

        impl ::ast::fold::Foldable for $name {
            fn try_fold_with<F: ast::fold::FallibleFolder>(
                self,
                ctx: &mut AstCtx,
                _folder: &mut F,
            ) -> Result<Self, <F as ::ast::fold::FallibleFolder>::Error> {
                let folded = <_ as ::ast::fold::Foldable>::try_fold_with(
                    <_ as ::common::Lookup>::lookup(&self, ctx).clone(),
                    ctx,
                    _folder,
                )?;
                let id = <AstCtx as ::common::Alloc<_>>::alloc(ctx, folded);
                Ok(id)
            }
        }
    };
}

newtype_id!(pub type ExpId = common::Idx<Exp> in exps);

newtype_id!(pub type StmId = common::Idx<Stm> in stms);

#[derive(Clone, Default, Debug)]
pub struct AstCtx {
    exps: la_arena::Arena<Exp>,
    stms: la_arena::Arena<Stm>,
}

impl AstCtx {
    pub fn new() -> Self {
        Self {
            exps: la_arena::Arena::new(),
            stms: la_arena::Arena::new(),
        }
    }
}

#[derive(Copy)]
#[derive_common]
/// Binary operator
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Eq,
    NEq,
    Lt,
}

#[derive(Copy)]
#[derive_common]
pub enum UnaryOp {
    Neg,
}

#[derive(Copy)]
#[derive_common]
pub enum Op {
    Binary(BinaryOp),
    Unary(UnaryOp),
}

impl From<BinaryOp> for Op {
    fn from(op: BinaryOp) -> Self {
        Op::Binary(op)
    }
}

impl From<UnaryOp> for Op {
    fn from(op: UnaryOp) -> Self {
        Op::Unary(op)
    }
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

struct Arg(u64);

impl fmt::Debug for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Arg").field(&self.0).finish()
    }
}

#[derive_common]
#[derive(SuperFoldable, DebugWithCtx)]
#[cfg_attr(any(feature = "serde", test), derive(common::serde::SerializeWithCtx))]
/// Expression
pub enum Exp {
    /// Integer
    Int(#[fold(identity)] i64),
    /// Identifier
    Id(Id),
    /// UnaryOp Exp
    UnaryOp(#[fold(identity)] UnaryOp, #[id] ExpId),
    /// Exp Op Exp
    BinOp(#[id] ExpId, #[fold(identity)] BinaryOp, #[id] ExpId),
    /// input
    Input,
    /// fun(Exp, Exp, ...)
    Call(#[id] ExpId, #[ctx] Vec<ExpId>),
    /// alloc Exp
    Alloc(#[id] ExpId),
    /// &Id
    Ref(Id),
    /// *Exp
    Deref(#[id] ExpId),
    /// { Id: Exp, ..., Id: Exp }
    Record(Record),
    /// Exp.Id
    FieldAccess(#[id] ExpId, Id),
    /// null
    Null,
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
#[derive(SuperFoldable, DebugWithCtx)]
#[cfg_attr(any(feature = "serde", test), derive(common::serde::SerializeWithCtx))]
/// Statement
pub enum Stm {
    /// Id = Exp;
    Assign(Id, #[id] ExpId),
    /// output Exp;
    Output(#[id] ExpId),
    /// Stm*
    Block(#[ctx] Vec<StmId>),
    /// if Exp { Stm } else { Stm }
    If(#[id] ExpId, #[id] StmId, #[id] Option<StmId>),
    /// while Exp { Stm }
    While(#[id] ExpId, #[id] StmId),
    /// var Id (, Id)*;
    Locals(Vec<Id>),
    /// return Exp;
    Return(#[id] ExpId),
    /// *Exp = Exp;
    Store(#[id] ExpId, #[id] ExpId),
    /// Id.Id = Exp;
    FieldUpdate(Id, Id, #[id] ExpId),
    /// (*Exp).Id = Exp;
    IndirectFieldUpdate(#[id] ExpId, Id, #[id] ExpId),
}

#[derive_common]
#[derive(SuperFoldable, DebugWithCtx)]
#[cfg_attr(any(feature = "serde", test), derive(common::serde::SerializeWithCtx))]
/// Function
///
/// name(arg, arg, arg) { locals? stm return exp }
pub struct Fun {
    pub name: Id,
    pub args: Vec<Id>,
    pub locals: Vec<Id>,
    #[ctx]
    pub body: Vec<StmId>,
    #[id]
    pub ret: ExpId,
}

impl Fun {
    pub fn is_main(&self) -> bool {
        self.name.0.as_str() == "main"
    }
}

#[derive_common]
#[derive(SuperFoldable, DebugWithCtx)]
#[cfg_attr(any(feature = "serde", test), derive(common::serde::SerializeWithCtx))]
pub struct Program {
    #[ctx]
    pub funs: Vec<Fun>,
}
#[derive_common]
#[derive(Foldable, DebugWithCtx)]
#[cfg_attr(any(feature = "serde", test), derive(common::serde::SerializeWithCtx))]
pub struct Field {
    pub name: Id,
    #[id]
    pub value: ExpId,
}

#[derive_common]
#[derive(Foldable, DebugWithCtx)]
#[cfg_attr(any(feature = "serde", test), derive(common::serde::SerializeWithCtx))]
pub struct Record {
    #[ctx]
    pub fields: Vec<Field>,
}

impl From<Vec<(Id, ExpId)>> for Record {
    fn from(fields: Vec<(Id, ExpId)>) -> Self {
        Record {
            fields: fields
                .into_iter()
                .map(|(name, value)| Field { name, value })
                .collect(),
        }
    }
}

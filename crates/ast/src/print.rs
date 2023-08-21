use common::Lookup;
use std::borrow::Cow;

use crate::{AstCtx, BinaryOp, Exp, Fun, Id, Record, Stm, UnaryOp};

pub type Str<'a> = Cow<'a, str>;

pub trait Print {
    fn print(&self, ctx: &AstCtx) -> Str<'_>;
}

impl Print for BinaryOp {
    fn print(&self, _ctx: &AstCtx) -> Str<'_> {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Gt => ">",
            BinaryOp::Eq => "==",
            BinaryOp::NEq => "!=",
            BinaryOp::Lt => "<",
        }
        .into()
    }
}

impl Print for UnaryOp {
    fn print(&self, _ctx: &AstCtx) -> Str<'_> {
        match self {
            UnaryOp::Neg => "-",
        }
        .into()
    }
}

macro_rules! p {
    ($($arg:tt)*) => {
        Cow::Owned(format!($( $arg )*).replace('\t', "    "))
    };
}

impl Print for Id {
    fn print(&self, _ctx: &AstCtx) -> Str<'_> {
        self.0.clone().into()
    }
}

impl Print for Record {
    fn print(&self, ctx: &AstCtx) -> Str<'_> {
        p!(
            "{{ {} }}",
            self.fields
                .iter()
                .map(
                    |ast::Field {
                         name: id,
                         value: exp,
                     }| p!("{}: {}", id.print(ctx), exp.lookup(ctx).print(ctx))
                )
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl Print for Exp {
    fn print(&self, ctx: &AstCtx) -> Str<'_> {
        match self {
            Exp::Int(i) => i.to_string().into(),
            Exp::Id(id) => id.print(ctx),
            // Exp::Paren(exp) => p!("({})", exp.lookup(ctx).print(ctx)),
            Exp::BinOp(lhs, op, rhs) => {
                p!(
                    "{} {} {}",
                    lhs.lookup(ctx).print(ctx),
                    op.print(ctx),
                    rhs.lookup(ctx).print(ctx)
                )
            }
            Exp::UnaryOp(op, exp) => p!("{}{}", op.print(ctx), exp.lookup(ctx).print(ctx)),
            Exp::Input => "input".into(),
            Exp::Call(fun, args) => p!(
                "{}({})",
                fun.lookup(ctx).print(ctx),
                args.iter()
                    .map(|arg| arg.lookup(ctx).print(ctx))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Exp::Alloc(exp) => p!("alloc {}", exp.lookup(ctx).print(ctx)),
            Exp::Ref(id) => p!("&{}", id.print(ctx)),
            Exp::Deref(exp) => p!("*{}", exp.lookup(ctx).print(ctx)),
            Exp::Record(rec) => rec.print(ctx),
            Exp::FieldAccess(exp, field) => {
                p!("{}.{}", exp.lookup(ctx).print(ctx), field.print(ctx))
            }
            Exp::Null => "null".into(),
        }
    }
}

impl Print for Stm {
    fn print(&self, ctx: &AstCtx) -> Str<'_> {
        match self {
            Stm::Assign(lhs, rhs) => {
                p!("{} = {};", lhs.print(ctx), rhs.lookup(ctx).print(ctx))
            }
            Stm::Output(exp) => {
                p!("output {};", exp.lookup(ctx).print(ctx))
            }
            Stm::Block(stms) => {
                p!(
                    "{{\n\t{}\n}}",
                    stms.iter()
                        .map(|stm| p!("\t{}", stm.lookup(ctx).print(ctx)))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            Stm::If(cond, then, else_) => {
                if let Some(else_) = else_ {
                    p!(
                        "if {} {} else {}",
                        cond.lookup(ctx).print(ctx),
                        then.lookup(ctx).print(ctx),
                        else_.lookup(ctx).print(ctx)
                    )
                } else {
                    p!(
                        "if {} {}",
                        cond.lookup(ctx).print(ctx),
                        then.lookup(ctx).print(ctx)
                    )
                }
            }
            Stm::While(cond, body) => {
                p!(
                    "while {} {}",
                    cond.lookup(ctx).print(ctx),
                    body.lookup(ctx).print(ctx)
                )
            }
            Stm::Locals(_) => todo!(),
            Stm::Return(_) => todo!(),
            Stm::Store(lhs, rhs) => {
                p!(
                    "*{} = {};",
                    lhs.lookup(ctx).print(ctx),
                    rhs.lookup(ctx).print(ctx)
                )
            }
            Stm::FieldUpdate(rec, field, rhs) => {
                p!(
                    "{}.{} = {};",
                    rec.print(ctx),
                    field.print(ctx),
                    rhs.lookup(ctx).print(ctx)
                )
            }
            Stm::IndirectFieldUpdate(rec, field, rhs) => {
                p!(
                    "(*{}).{} = {};",
                    rec.lookup(ctx).print(ctx),
                    field.print(ctx),
                    rhs.lookup(ctx).print(ctx)
                )
            }
        }
    }
}

impl Print for Fun {
    fn print(&self, ctx: &AstCtx) -> Str<'_> {
        let locals = if self.locals.is_empty() {
            "".into()
        } else {
            p!(
                "var {};",
                self.locals
                    .iter()
                    .map(|local| local.print(ctx))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        p!(
            "{}({}) {{\n\t{}\n{}\n\treturn {};\n}}",
            self.name.print(ctx),
            self.args
                .iter()
                .map(|arg| arg.print(ctx))
                .collect::<Vec<_>>()
                .join(", "),
            locals,
            self.body
                .iter()
                .map(|stm| p!("\t{}", stm.lookup(ctx).print(ctx)))
                .collect::<Vec<_>>()
                .join("\n"),
            self.ret.lookup(ctx).print(ctx),
        )
    }
}

impl Print for crate::Program {
    fn print(&self, ctx: &AstCtx) -> Str<'_> {
        self.funs
            .iter()
            .map(|fun| fun.print(ctx))
            .collect::<Vec<_>>()
            .join("\n")
            .into()
    }
}

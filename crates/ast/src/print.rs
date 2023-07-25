use std::borrow::Cow;

use crate::{Exp, Fun, Id, Op, Record, Stm};

pub type Str<'a> = Cow<'a, str>;

pub trait Print {
    fn print(&self) -> Str<'_>;
}

impl Print for Op {
    fn print(&self) -> Str<'_> {
        match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Gt => ">",
            Op::Eq => "==",
            Op::NEq => "!=",
            Op::Lt => "<",
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
    fn print(&self) -> Str<'_> {
        self.0.clone().into()
    }
}

impl Print for Record {
    fn print(&self) -> Str<'_> {
        p!(
            "{{ {} }}",
            self.fields
                .iter()
                .map(|(id, exp)| p!("{}: {}", id.print(), exp.print()))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl Print for Exp {
    fn print(&self) -> Str<'_> {
        match self {
            Exp::Int(i) => i.to_string().into(),
            Exp::Id(id) => id.print(),
            Exp::Paren(exp) => p!("({})", exp.print()),
            Exp::BinOp(lhs, op, rhs) => {
                p!("{} {} {}", lhs.print(), op.print(), rhs.print())
            }
            Exp::Input => "input".into(),
            Exp::Call(fun, args) => p!(
                "{}({})",
                fun.print(),
                args.iter()
                    .map(|arg| arg.print())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Exp::Alloc(exp) => p!("alloc {}", exp.print()),
            Exp::Ref(id) => p!("&{}", id.print()),
            Exp::Deref(exp) => p!("*{}", exp.print()),
            Exp::Record(rec) => rec.print(),
            Exp::FieldAccess(exp, field) => {
                p!("{}.{}", exp.print(), field.print())
            }
            Exp::Null => "null".into(),
            Exp::Error => "error".into(),
        }
    }
}

impl Print for Stm {
    fn print(&self) -> Str<'_> {
        match self {
            Stm::Assign(lhs, rhs) => {
                p!("{} = {};", lhs.print(), rhs.print())
            }
            Stm::Output(exp) => {
                p!("output {};", exp.print())
            }
            Stm::Block(stms) => {
                p!(
                    "{{\n\t{}\n}}",
                    stms.iter()
                        .map(|stm| p!("\t{}", stm.print()))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            Stm::If(cond, then, else_) => {
                if let Some(else_) = else_ {
                    p!(
                        "if {} {} else {}",
                        cond.print(),
                        then.print(),
                        else_.print()
                    )
                } else {
                    p!("if {} {}", cond.print(), then.print())
                }
            }
            Stm::While(cond, body) => {
                p!("while {} {}", cond.print(), body.print())
            }
            Stm::Locals(_) => todo!(),
            Stm::Return(_) => todo!(),
            Stm::Store(lhs, rhs) => {
                p!("*{} = {};", lhs.print(), rhs.print())
            }
            Stm::FieldUpdate(rec, field, rhs) => {
                p!("{}.{} = {};", rec.print(), field.print(), rhs.print())
            }
            Stm::IndirectFieldUpdate(rec, field, rhs) => {
                p!("(*{}).{} = {};", rec.print(), field.print(), rhs.print())
            }
        }
    }
}

impl Print for Fun {
    fn print(&self) -> Str<'_> {
        let locals = if self.locals.is_empty() {
            "".into()
        } else {
            p!(
                "var {};",
                self.locals
                    .iter()
                    .map(|local| local.print())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        p!(
            "{}({}) {{\n\t{}\n{}\n\treturn {};\n}}",
            self.name.print(),
            self.args
                .iter()
                .map(|arg| arg.print())
                .collect::<Vec<_>>()
                .join(", "),
            locals,
            self.body
                .iter()
                .map(|stm| p!("\t{}", stm.print()))
                .collect::<Vec<_>>()
                .join("\n"),
            self.ret.print(),
        )
    }
}

impl Print for crate::Program {
    fn print(&self) -> Str<'_> {
        self.funs
            .iter()
            .map(|fun| fun.print())
            .collect::<Vec<_>>()
            .join("\n")
            .into()
    }
}

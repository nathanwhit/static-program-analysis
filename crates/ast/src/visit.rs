// use crate::{Exp, Fun, Id, Program, Stm};
// pub mod walk {
//     use crate::Program;

//     use super::{Visitor, *};
//     pub fn exp<V: Visitor>(exp: &Exp, visitor: &mut V) {
//         match exp {
//             Exp::Int(_) => {}
//             Exp::Id(id) => {
//                 visitor.visit_id(id);
//             }
//             Exp::Paren(exp) => {
//                 visitor.visit_exp(exp);
//             }
//             Exp::BinOp(exp1, _op, exp2) => {
//                 visitor.visit_exp(exp1);
//                 visitor.visit_exp(exp2);
//             }
//             Exp::Input => {}
//             Exp::Call(fun, args) => {
//                 visitor.visit_exp(fun);
//                 for arg in args {
//                     visitor.visit_exp(arg);
//                 }
//             }
//             Exp::Alloc(alloc) => {
//                 visitor.visit_exp(alloc);
//             }
//             Exp::Ref(id) => {
//                 visitor.visit_id(id);
//             }
//             Exp::Deref(exp) => {
//                 visitor.visit_exp(exp);
//             }
//             Exp::Record(record) => {
//                 for (id, exp) in &record.fields {
//                     visitor.visit_id(id);
//                     visitor.visit_exp(exp);
//                 }
//             }
//             Exp::Null => {}
//             Exp::FieldAccess(exp, id) => {
//                 visitor.visit_exp(exp);
//                 visitor.visit_id(id);
//             }
//             Exp::Error => {}
//         }
//     }

//     pub fn stm<V: Visitor>(stm: &Stm, visitor: &mut V) {
//         match stm {
//             Stm::Assign(id, exp) => {
//                 visitor.visit_id(id);
//                 visitor.visit_exp(exp);
//             }
//             Stm::Output(exp) => {
//                 visitor.visit_exp(exp);
//             }
//             Stm::Block(stms) => {
//                 for stm in stms {
//                     visitor.visit_stm(stm);
//                 }
//             }
//             Stm::If(cond, then, else_) => {
//                 visitor.visit_exp(cond);
//                 visitor.visit_stm(then);
//                 if let Some(else_) = else_ {
//                     visitor.visit_stm(else_);
//                 }
//             }
//             Stm::While(cond, body) => {
//                 visitor.visit_exp(cond);
//                 visitor.visit_stm(body);
//             }
//             Stm::Locals(idents) => {
//                 for ident in idents {
//                     visitor.visit_id(ident);
//                 }
//             }
//             Stm::Return(exp) => {
//                 visitor.visit_exp(exp);
//             }
//             Stm::Store(lhs, rhs) => {
//                 visitor.visit_exp(lhs);
//                 visitor.visit_exp(rhs);
//             }
//             Stm::FieldUpdate(record, field, value) => {
//                 visitor.visit_id(record);
//                 visitor.visit_id(field);
//                 visitor.visit_exp(value);
//             }
//             Stm::IndirectFieldUpdate(record_ptr, field, value) => {
//                 visitor.visit_exp(record_ptr);
//                 visitor.visit_id(field);
//                 visitor.visit_exp(value);
//             }
//         }
//     }

//     pub fn fun<V: Visitor>(fun: &Fun, visitor: &mut V) {
//         visitor.visit_id(&fun.name);
//         for arg in &fun.args {
//             visitor.visit_id(arg);
//         }
//         for local in &fun.locals {
//             visitor.visit_id(local);
//         }
//         for stm in &fun.body {
//             visitor.visit_stm(&stm);
//         }
//         visitor.visit_exp(&fun.ret);
//     }

//     pub fn program<V: Visitor>(program: &Program, visitor: &mut V) {
//         for fun in &program.funs {
//             visitor.visit_fun(fun);
//         }
//     }
// }

// pub trait Visitor: Sized {
//     fn visit_exp(&mut self, exp: &Exp) {
//         walk::exp(exp, self);
//     }
//     fn visit_stm(&mut self, stm: &Stm) {
//         walk::stm(stm, self);
//     }
//     fn visit_fun(&mut self, fun: &Fun) {
//         walk::fun(fun, self);
//     }
//     fn visit_id(&mut self, _id: &Id) {}

//     fn visit_program(&mut self, program: &Program) {
//         walk::program(program, self);
//     }
// }

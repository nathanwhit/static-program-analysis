use ast::Id;
use core::fmt;
use std::{
    cell::RefCell,
    collections::HashMap,
    io::{BufRead, BufReader, Write},
    rc::Rc,
};
use thiserror::Error;

pub struct ErrAdapter<T>(Rc<RefCell<T>>);

impl<T> Clone for ErrAdapter<T> {
    fn clone(&self) -> Self {
        ErrAdapter(self.0.clone())
    }
}

impl<T> std::fmt::Debug for ErrAdapter<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.borrow().fmt(f)
    }
}

impl<T> std::fmt::Display for ErrAdapter<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.borrow().fmt(f)
    }
}

impl<T> std::error::Error for ErrAdapter<T> where T: std::error::Error {}

impl<T> From<T> for ErrAdapter<T> {
    fn from(t: T) -> Self {
        ErrAdapter(Rc::new(RefCell::new(t)))
    }
}

impl<T> PartialEq for ErrAdapter<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

trait ErrAdapt {
    type Output;
    fn adapt_err(self) -> Self::Output;
}

impl<T, E> ErrAdapt for std::result::Result<T, E>
where
    E: std::error::Error + 'static,
{
    type Output = std::result::Result<T, ErrAdapter<E>>;
    fn adapt_err(self) -> Self::Output {
        self.map_err(ErrAdapter::from)
    }
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum Error {
    #[error("unresolved identifier: {0}")]
    UnresolvedId(Id),

    #[error("invalid operand: {0} for {1:?}")]
    InvalidOperand(String, Option<ast::Op>),

    #[error("io failed: {0}")]
    IoFailed(#[from] ErrAdapter<std::io::Error>),

    #[error("invalid field access: {0}")]
    InvalidFieldAccess(String),

    #[error("invalid input: {0:?}")]
    InvalidInput(String),

    #[error("no main")]
    NoMain,
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub type ValueRef = Rc<RefCell<Value>>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
#[cfg_attr(any(test, feature = "serde"), derive(serde::Serialize))]
pub struct Record {
    fields: Vec<(Id, Value)>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
#[cfg_attr(any(test, feature = "serde"), derive(serde::Serialize))]
pub enum Value {
    Int(i64),
    Record(Record),
    Ref(ValueRef),
    Boolean(bool),
    Fun(ast::Fun),
    Null,
}

impl Value {
    fn name(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Record(_) => "record",
            Value::Ref(_) => "ref",
            Value::Boolean(_) => "bool",
            Value::Fun(_) => "fun",
            Value::Null => "null",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(int) => write!(f, "{}", int),
            Value::Record(record) => {
                write!(f, "{{ ")?;
                for (id, value) in &record.fields {
                    write!(f, "{}: {}, ", id, value)?;
                }
                write!(f, "}}")
            }
            Value::Ref(ref_) => write!(f, "0x{:x}", ref_.as_ptr() as usize),
            Value::Boolean(bool) => write!(f, "{}", bool),
            Value::Fun(fun) => write!(f, "{}", fun.name),
            Value::Null => write!(f, "null"),
        }
    }
}

impl Value {
    fn as_int(&self, op: ast::Op) -> Result<i64> {
        match self {
            Value::Int(int) => Ok(*int),
            _ => Err(Error::InvalidOperand("int".to_string(), Some(op))),
        }
    }
    fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(bool) => *bool,
            Value::Int(int) => *int != 0,
            Value::Record(_) => true,
            Value::Ref(_) => true,
            Value::Fun(_) => true,
            Value::Null => false,
        }
    }

    fn as_record(&self) -> Result<&Record> {
        match self {
            Value::Record(record) => Ok(record),
            _ => Err(Error::InvalidOperand(
                format!("wanted record but got {} ({self:?})", self.name()),
                None,
            )),
        }
    }

    fn as_record_mut(&mut self) -> Result<&mut Record> {
        match self {
            Value::Record(record) => Ok(record),
            _ => Err(Error::InvalidOperand(
                format!("wanted record but got {} ({self:?})", self.name()),
                None,
            )),
        }
    }

    fn as_ref(&self) -> Result<ValueRef> {
        match self {
            Value::Ref(ref_) => Ok(ref_.clone()),
            _ => Err(Error::InvalidOperand("ref".to_string(), None)),
        }
    }

    fn as_fun(&self) -> Result<&ast::Fun> {
        match self {
            Value::Fun(fun) => Ok(fun),
            _ => Err(Error::InvalidOperand("fun".to_string(), None)),
        }
    }
}

#[derive(Clone, Default)]
struct Interpreter<I, O> {
    locals: HashMap<Id, Vec<Value>>,
    functions: HashMap<Id, ast::Fun>,
    input: I,
    output: O,
}

impl<I, O> fmt::Debug for Interpreter<I, O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Interpreter")
            .field("locals", &self.locals)
            .field("functions", &self.functions)
            .finish()
    }
}

impl<I, O> Interpreter<I, O> {
    #[tracing::instrument(skip(self))]
    fn push_local(&mut self, id: Id, value: Value) {
        self.locals.entry(id).or_default().push(value);
    }
    #[tracing::instrument(skip(self))]
    fn pop_local(&mut self, id: Id) -> Option<Value> {
        self.locals.entry(id).or_default().pop()
    }
    #[tracing::instrument(skip(self))]
    fn store_local(&mut self, id: Id, value: Value) -> Result<()> {
        self.locals
            .entry(id.clone())
            .or_default()
            .last_mut()
            .ok_or(Error::UnresolvedId(id.clone()))
            .map(|v| *v = value)?;
        Ok(())
    }
}

impl Interpreter<BufReader<std::io::Stdin>, std::io::Stdout> {
    fn new(program: &ast::Program) -> Self {
        Self::with_buf_io(program, BufReader::new(std::io::stdin()), std::io::stdout())
    }
}

impl<I, O> Interpreter<I, O>
where
    I: BufRead,
    O: Write,
{
    fn with_buf_io(program: &ast::Program, input: I, output: O) -> Self {
        let mut interp = Self {
            input,
            output,
            functions: HashMap::new(),
            locals: HashMap::new(),
        };
        for fun in &program.funs {
            interp.functions.insert(fun.name.clone(), fun.clone());
        }
        interp
    }

    #[tracing::instrument]
    fn eval(&mut self) -> Result<Value> {
        let main = self
            .functions
            .get(&Id::from("main"))
            .ok_or(Error::NoMain)?
            .clone();
        self.eval_call(&main, &[])
    }

    #[tracing::instrument]
    fn resolve(&self, id: &Id) -> Result<Value> {
        tracing::debug!(?id, "looking up id");
        if let Some(values) = self.locals.get(id) {
            return values
                .last()
                .cloned()
                .ok_or_else(|| Error::UnresolvedId(id.clone()));
        }

        if let Some(fun) = self.functions.get(id) {
            return Ok(Value::Fun(fun.clone()));
        }

        Err(Error::UnresolvedId(id.clone()))
    }

    #[tracing::instrument]
    fn resolve_local(&mut self, id: &Id) -> Result<&mut Value> {
        if let Some(values) = self.locals.get_mut(id) {
            return values
                .last_mut()
                .ok_or_else(|| Error::UnresolvedId(id.clone()));
        }

        Err(Error::UnresolvedId(id.clone()))
    }

    #[tracing::instrument]
    fn eval_call(&mut self, fun: &ast::Fun, args: &[Value]) -> Result<Value> {
        if args.len() != fun.args.len() {
            panic!("wrong number of arguments");
            // return None;
        }

        for (arg, value) in fun.args.iter().zip(args) {
            self.push_local(arg.clone(), value.clone());
        }

        for local in &fun.locals {
            self.push_local(local.clone(), Value::Null);
        }

        for stm in &fun.body {
            self.eval_stm(stm)?;
        }

        let ret = self.eval_exp(&fun.ret)?;

        for local in &fun.locals {
            self.pop_local(local.clone());
        }

        for arg in &fun.args {
            self.pop_local(arg.clone());
        }

        Ok(ret)
    }

    #[tracing::instrument(skip(self))]
    fn eval_exp(&mut self, exp: &ast::Exp) -> Result<Value> {
        Ok(match exp {
            ast::Exp::Int(int) => Value::Int(*int),
            ast::Exp::Id(id) => self.resolve(id)?,
            ast::Exp::Paren(inner) => self.eval_exp(inner)?,
            ast::Exp::BinOp(lhs, op, rhs) => {
                let op = *op;
                let lhs = self.eval_exp(lhs)?;
                let rhs = self.eval_exp(rhs)?;
                use ast::Op::*;
                match op {
                    Add | Sub | Mul | Div => {
                        let lhs = lhs.as_int(op)?;
                        let rhs = rhs.as_int(op)?;
                        Value::Int(match op {
                            Add => lhs + rhs,
                            Sub => lhs - rhs,
                            Mul => lhs * rhs,
                            Div => lhs / rhs,
                            _ => unreachable!(),
                        })
                    }
                    Gt => {
                        let lhs = lhs.as_int(op)?;
                        let rhs = rhs.as_int(op)?;
                        Value::Boolean(lhs > rhs)
                    }
                    Lt => {
                        let lhs = lhs.as_int(op)?;
                        let rhs = rhs.as_int(op)?;
                        Value::Boolean(lhs < rhs)
                    }
                    Eq => Value::Boolean(lhs == rhs),
                    NEq => Value::Boolean(lhs != rhs),
                }
            }
            ast::Exp::Input => {
                let mut input = String::new();
                self.input.read_line(&mut input).adapt_err()?;
                Value::Int(
                    input
                        .trim()
                        .parse()
                        .map_err(|_| Error::InvalidInput(input))?,
                )
            }
            ast::Exp::Call(fun, args) => {
                let fun = self.eval_exp(fun)?;
                let fun = fun.as_fun()?;
                let args = args
                    .iter()
                    .map(|arg| self.eval_exp(arg))
                    .collect::<Result<Vec<_>>>()?;
                self.eval_call(fun, &args)?
            }
            ast::Exp::Alloc(exp) => {
                let val = self.eval_exp(exp)?;

                Value::Ref(Rc::new(RefCell::new(val)))
            }
            ast::Exp::Ref(id) => {
                let val = self.resolve(id)?;
                let val = val.as_ref()?;
                Value::Ref(val)
            }
            ast::Exp::Deref(exp) => {
                let refer = self.eval_exp(exp)?;
                let refer = refer.as_ref()?;
                let derefed = refer.borrow().clone();
                derefed
            }
            ast::Exp::Record(rec) => {
                let fields = rec
                    .fields
                    .iter()
                    .map(|(id, exp)| Ok((id.clone(), self.eval_exp(exp)?)))
                    .collect::<Result<Vec<_>>>()?;
                Value::Record(Record { fields })
            }
            ast::Exp::FieldAccess(rec, field) => {
                let rec = self.eval_exp(rec)?;
                let rec = rec.as_record()?;
                let field = rec
                    .fields
                    .iter()
                    .find(|(id, _)| id == field)
                    .ok_or_else(|| Error::InvalidFieldAccess(field.to_string()))?;
                field.1.clone()
            }
            ast::Exp::Null => Value::Null,
            ast::Exp::Error => unreachable!(),
        })
    }

    #[tracing::instrument]
    fn eval_stm(&mut self, stm: &ast::Stm) -> Result<()> {
        match stm {
            ast::Stm::Assign(lhs, rhs) => {
                let val = self.eval_exp(rhs)?;
                self.store_local(lhs.clone(), val)?;
            }
            ast::Stm::Output(exp) => {
                let val = self.eval_exp(exp)?;
                writeln!(self.output, "{}", val).adapt_err()?;
            }
            ast::Stm::Block(stms) => {
                for stm in stms {
                    self.eval_stm(stm)?;
                }
            }
            ast::Stm::If(cond, then, else_) => {
                let cond = self.eval_exp(cond)?;
                if cond.is_truthy() {
                    self.eval_stm(then)?;
                } else if let Some(else_) = else_ {
                    self.eval_stm(else_)?;
                }
            }
            ast::Stm::While(cond, body) => {
                while self.eval_exp(cond)?.is_truthy() {
                    self.eval_stm(body)?;
                }
            }
            ast::Stm::Locals(_) => todo!(),
            ast::Stm::Return(_) => todo!(),
            ast::Stm::Store(lhs, rhs) => {
                let rhs = self.eval_exp(rhs)?;
                let lhs = self.eval_exp(lhs)?;
                let lhs = lhs.as_ref()?;
                *lhs.borrow_mut() = rhs;
            }
            ast::Stm::FieldUpdate(rec, field, rhs) => {
                let rhs = self.eval_exp(rhs)?;
                let rec = self.resolve_local(rec)?;
                let rec = rec.as_record_mut()?;
                let field = rec
                    .fields
                    .iter_mut()
                    .find(|(id, _)| id == field)
                    .ok_or_else(|| Error::InvalidFieldAccess(field.to_string()))?;
                field.1 = rhs;
            }
            ast::Stm::IndirectFieldUpdate(ptr, field, rhs) => {
                let ptr = self.eval_exp(ptr)?;
                let ptr = ptr.as_ref()?;
                let mut ptr = ptr.borrow_mut();
                let rec = ptr.as_record_mut()?;
                let field_val = rec
                    .fields
                    .iter_mut()
                    .find(|(id, _)| id == field)
                    .ok_or_else(|| Error::InvalidFieldAccess(field.to_string()))?;
                field_val.1 = self.eval_exp(rhs)?;
            }
        }

        Ok(())
    }
}

pub fn program(program: &ast::Program) -> Result<Value> {
    let mut interpreter = Interpreter::new(program);
    interpreter.eval()
}

#[cfg(test)]
mod test {
    use std::io::BufReader;
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::Registry;
    use tracing_tree::HierarchicalLayer;

    use crate::Value;

    struct InputLines(Vec<String>, usize);
    impl std::io::Read for InputLines {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            let line = self.1;
            self.1 += 1;
            if line >= self.0.len() {
                return Ok(0);
            }
            let line = self.0[line].as_bytes();
            let len = line.len();
            buf[..len].copy_from_slice(line);
            buf[len] = b'\n';
            Ok(len)
        }
    }
    struct Output(Vec<u8>);

    impl std::io::Write for Output {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.extend_from_slice(buf);
            self.0.push(b'\n');
            Ok(buf.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    #[track_caller]
    fn run(prog: &str) -> Result<Value, Box<dyn std::error::Error + 'static>> {
        let reg = Registry::default().with(HierarchicalLayer::new(2));
        let _trace = tracing::subscriber::set_default(reg);
        let prog = parse::parse(prog).unwrap();
        let input = InputLines(vec![], 0);
        let output = Output(vec![]);
        let mut interp = super::Interpreter::with_buf_io(&prog, BufReader::new(input), output);
        interp.eval().map_err(|e| e.into())
    }
    macro_rules! test {
        (@name = $name: ident, @in = $in: expr, @out = $out: expr, @fail = ) => {
            #[test]
            fn $name() {
                assert_eq!(run($in).unwrap(), $out);
            }
        };
        (@name = $name: ident, @in = $in: expr, @out = $out: expr, @fail = $fail: ident) => {
            #[test]
            fn $name() {
                assert_eq!(run($in).unwrap_err(), $out);
            }
        };
        ($($(#$fail: ident )? $name: ident : $in: expr => $out: expr),+ $(,)?) => {

            $(
                test!(@name = $name, @in = $in, @out = $out, @fail = $($fail)?);
            )+

        };
    }

    fn fact(n: i64) -> i64 {
        if n == 0 {
            1
        } else {
            n * fact(n - 1)
        }
    }

    test! {
        eval_basic : "main() {
            return 1;
        }
        " => Value::Int(1),

        eval_add : "main() {
            return 1 + 2;
        }
        " => Value::Int(3),

        eval_sub : "main() {
            return 1 - 2;
        }
        " => Value::Int(-1),

        eval_mul : "main() {
            return 2 * 3;
        }
        " => Value::Int(6),

        eval_div : "main() {
            return 6 / 2;
        }
        " => Value::Int(3),

        eval_gt : "main() {
            return 1 > 2;
        }
        " => Value::Boolean(false),

        eval_iterate : "
            iterate(n) {
                var f;
                f = 1;
                while (n>0) {
                    f = f*n;
                    n = n-1; 
                }
                return f; 
            }
            main() {
                return iterate(5);
            }
        " => Value::Int(fact(5)),

        eval_recurse_fact: "
            recurse(n) {
                var f;
                if (n==0) { f=1; }
                else { f=n*recurse(n-1); }
                return f;
            }
            main() {
                return recurse(5);
            }
        " => Value::Int(fact(5)),
    }
}

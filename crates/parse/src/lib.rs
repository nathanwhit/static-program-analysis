#![feature(rustc_attrs)]

use ast::{Exp, Fun, Id, Op, Program, Record, Stm};
use chumsky::{container::OrderedSeq, pratt::*, prelude::*, text::Char};
use common::Boxed;

type InputTy<'a> = &'a str;
type Tok<'a> = <InputTy<'a> as chumsky::input::Input<'a>>::Token;

macro_rules! parser {
    ($lt: lifetime, $out: ty) => {
        impl ::chumsky::prelude::Parser<
            $lt,
            $crate::InputTy<$lt>,
            $out,
            ::chumsky::extra::Err<
                ::chumsky::prelude::Rich<
                    'a,
                    <$crate::InputTy<$lt> as ::chumsky::input::Input<$lt>>::Token
                >
            >
        > + Clone
    };
}

fn tag<'a, T>(seq: T) -> parser!('a, T)
where
    T: OrderedSeq<'a, Tok<'a>> + Clone,
{
    just(seq).padded()
}

fn id<'a>() -> parser!('a, Id) {
    let valid = move |c: &Tok<'a>| c.is_alphanumeric() || *c == '_';
    any()
        .filter(move |c: &Tok<'a>| valid(c) && !c.is_digit(10))
        .then(any().filter(valid).repeated())
        .map_slice(Id::from)
        .padded()
}

fn locals<'a>() -> parser!('a, Vec<Id>) {
    tag("var")
        .ignore_then(
            id().separated_by(tag(','))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .or(empty().map(|_| vec![]))
}

fn int<'a>() -> parser!('a, i64) {
    tag('-')
        .or(any().filter(|c: &Tok<'a>| c.is_digit(10)))
        .then(any().filter(|c: &Tok<'a>| c.is_digit(10)).repeated())
        .map_slice(|s: InputTy<'a>| s.parse().unwrap())
}

fn record<'a>(exp: parser!('a, Exp)) -> parser!('a, Record) {
    id().then_ignore(tag(':'))
        .then(exp)
        .separated_by(tag(','))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(tag('{'), tag('}'))
        .map(Record::from)
}

fn exp<'a>() -> parser!('a, Exp) {
    recursive(|exp| {
        let paren = exp
            .clone()
            .delimited_by(tag('('), tag(')'))
            .map(|e: Exp| Exp::Paren(e.into_box()));
        let input = tag("input").to(Exp::Input);
        let null = tag("null").to(Exp::Null);
        let int = int().padded().map(Exp::Int);
        let record = record(exp.clone()).map(Exp::Record);
        let ident = id().map(Exp::Id);
        let non_call_atom = choice((
            int.clone().boxed(),
            null.clone().boxed(),
            input.clone().boxed(),
            paren.clone().boxed(),
            ident.clone().boxed(),
            record.clone().boxed(),
        ));

        let call = non_call_atom
            .clone()
            .padded()
            .then(
                exp.clone()
                    .padded()
                    .separated_by(tag(','))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(tag('('), tag(')')),
            )
            .map(|(fun, args)| Exp::Call(fun.into_box(), args));
        let field_access = non_call_atom
            .clone()
            .then_ignore(tag('.'))
            .then(id())
            .map(|(e, f)| Exp::FieldAccess(e.into_box(), f));
        let atom = choice((
            call.boxed(),
            field_access.boxed(),
            non_call_atom.clone().boxed(),
        ));

        macro_rules! infix_op {
            ($symbol: expr, $strength: expr, $op: expr) => {
                left_infix(tag($symbol), $strength, |l: Exp, r: Exp| {
                    Exp::BinOp(l.into_box(), $op, r.into_box())
                })
            };
        }

        let operator = choice((
            infix_op!('<', 1, Op::Lt),
            infix_op!('>', 1, Op::Gt),
            infix_op!("==", 1, Op::Eq),
            infix_op!("!=", 1, Op::NEq),
            infix_op!('+', 2, Op::Add),
            infix_op!('-', 2, Op::Sub),
            infix_op!('*', 3, Op::Mul),
            infix_op!('/', 3, Op::Div),
        ));
        let unary = choice((
            prefix(tag('&'), 4, |e: Exp| match e {
                Exp::Id(id) => Exp::Ref(id),
                _ => Exp::Error,
            }),
            prefix(tag('*'), 5, |e: Exp| Exp::Deref(e.into_box())),
            prefix(just("alloc"), 5, |e: Exp| Exp::Alloc(e.into_box())),
        ));

        let pratt = atom.clone().padded().pratt(operator).with_prefix_ops(unary);
        choice((
            // field_access,
            pratt.try_map(|e, span| match e {
                Exp::Error => Err(Rich::custom(
                    span,
                    "ref (`&`) must be followed by an identifier",
                )),
                _ => Ok(e),
            }),
            atom.clone(),
        ))
    })
}

fn stm<'a>() -> parser!('a, Stm) {
    recursive(|stm| {
        let assign = id()
            .padded()
            .then_ignore(tag('='))
            .then(exp())
            .then_ignore(tag(';'))
            .map(|(id, exp)| Stm::Assign(id, exp));

        let output = just("output")
            .padded()
            .ignore_then(exp())
            .then_ignore(tag(';'))
            .map(Stm::Output);

        let block = stm
            .clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(tag('{'), tag('}'))
            .map(Stm::Block);

        let if_ = tag("if").ignore_then(exp()).then(block.clone()).padded();
        let if_else = if_
            .clone()
            .then(tag("else").ignore_then(block.clone()))
            .map(|((exp, then), else_)| Stm::If(exp, then.into_box(), Some(else_.into_box())));
        let if_no_else = if_.map(|(exp, then)| Stm::If(exp, then.into_box(), None));
        let if_maybe_else = if_else.or(if_no_else);

        let while_ = tag("while")
            .ignore_then(exp())
            .then(block.clone())
            .map(|(exp, body)| Stm::While(exp, body.into_box()));

        let store = tag('*')
            .ignore_then(exp())
            .then_ignore(tag('='))
            .then(exp())
            .then_ignore(tag(';'))
            .map(|(ptr, val)| Stm::Store(ptr, val));

        let field_update = id()
            .then_ignore(tag('.'))
            .then(id())
            .then_ignore(tag('='))
            .then(exp())
            .then_ignore(tag(';'))
            .map(|((rec, field), val)| Stm::FieldUpdate(rec, field, val));

        let indirect_field_update = tag('(')
            .ignore_then(tag('*').ignore_then(exp()))
            .then_ignore(tag(')'))
            .then_ignore(tag('.'))
            .then(id())
            .then_ignore(tag('='))
            .then(exp())
            .then_ignore(tag(';'))
            .map(|((rec, field), val)| Stm::IndirectFieldUpdate(rec, field, val));

        choice((
            assign,
            output,
            if_maybe_else,
            while_,
            store,
            field_update,
            indirect_field_update,
        ))
    })
}

fn fun<'a>() -> parser!('a, Fun) {
    let name = id();
    let args = id()
        .separated_by(tag(','))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(tag('('), tag(')'));
    let locals = locals().then_ignore(tag(';')).or(empty().map(|_| vec![]));
    let body = stm().padded().repeated().collect::<Vec<_>>();
    let ret = tag("return").ignore_then(exp()).then_ignore(tag(';'));

    name.then(args)
        .then(locals.then(body).then(ret).delimited_by(tag('{'), tag('}')))
        .map(|((name, args), ((locals, body), ret))| Fun {
            name,
            args,
            locals,
            body,
            ret,
        })
}

pub fn program<'a>() -> parser!('a, Program) {
    fun()
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .map(|funs| Program { funs })
}

pub fn parse<'a>(input: &'a str) -> Result<Program, Vec<Rich<'a, Tok<'a>>>> {
    program().parse(input).into_result()
}

#[cfg(test)]
mod tests {
    use ast::{Fun, Program};

    use super::{Boxed, Exp, Id, Op, Parser, Record, Stm};

    #[track_caller]
    fn run<'a, T>(input: &'a str, parser: parser!('a, T), expected: T)
    where
        T: PartialEq + std::fmt::Debug,
    {
        let out = parser.parse(input).unwrap();
        assert_eq!(out, expected);
    }

    fn bin_op(lhs: impl Into<Exp>, op: Op, rhs: impl Into<Exp>) -> Exp {
        Exp::BinOp(lhs.into().into_box(), op, rhs.into().into_box())
    }

    fn int(i: i64) -> Exp {
        Exp::Int(i)
    }

    fn id(s: &str) -> Id {
        Id::from(s)
    }

    fn exp(i: impl Into<Exp>) -> Box<Exp> {
        i.into().into_box()
    }

    fn fun(
        name: &str,
        args: Vec<&str>,
        locals: Vec<&str>,
        body: Vec<Stm>,
        ret: impl Into<Exp>,
    ) -> Fun {
        Fun {
            name: Id::from(name),
            args: args.into_iter().map(Id::from).collect(),
            locals: locals.into_iter().map(Id::from).collect(),
            body,
            ret: ret.into(),
        }
    }

    macro_rules! block {
        (box $($stm: expr),* $(,)?) => {
            Stm::Block(vec![$($stm),*]).into_box()
        };
        ($($stm: expr),* $(,)?) => {
            Stm::Block(vec![$($stm),*])
        };
    }

    macro_rules! test {
        (@fun = $fun: ident, @name = $name: ident, @in = $in: expr, @out = $out: expr, @fail = ) => {
            #[test]
            fn $name() {
                run($in, super::$fun(), $out);
            }
        };
        (@fun = $fun: ident, @name = $name: ident, @in = $in: expr, @out = $out: expr, @fail = $fail: ident) => {
            #[test]
            fn $name() {
                assert!(super::$fun().parse($in).has_errors());
            }
        };
        ($($fun: ident $(-)+ $($(#$fail: ident )? $name: ident : $in: expr => $out: expr),+ $(,)?);* $(;)?) => {
            $(
                $(
                    test!(@fun = $fun, @name = $name, @in = $in, @out = $out, @fail = $($fail)?);
                )+
            )*
        };
    }

    test! {
        int
        ---
        int_works: "123" => 123,
        int_works_with_neg: "-123" => -123,
        int_zero: "0" => 0,
        #fail int_fails_with_char: "a123" => bad;

        id
        -----
        ident_works: "abc" => Id::from("abc"),
        ident_works_with_digits: "abc123" => Id::from("abc123"),
        ident_works_with_underscore: "abc_123" => Id::from("abc_123"),
        #fail ident_fails_with_digit_first: "123abc" => bad;

        locals
        ------
        locals_works: "var a, b, c" => vec![Id::from("a"), Id::from("b"), Id::from("c")],
        locals_with_ws: "var a , b , c" => vec![Id::from("a"), Id::from("b"), Id::from("c")],
        locals_trailing_comma: "var a, b, c," => vec![Id::from("a"), Id::from("b"), Id::from("c")],
        locals_one: "var a" => vec![Id::from("a")];

        exp
        ---
        exp_int: "123" => Exp::Int(123),
        exp_id: "abc" => id("abc").into(),
        exp_paren: "( 123)" => Exp::Paren(exp(123)),
        exp_binop: "123 + 456" => bin_op(123, Op::Add, 456),
        exp_input: "input" => Exp::Input,
        exp_call: "fun(123, 456)" => Exp::Call(exp("fun"), vec![int(123), int(456)]),
        exp_alloc: "alloc 123" => Exp::Alloc(exp(123)),
        exp_ref: "&abc" => Exp::Ref(id("abc")),
        #fail exp_ref_not_id: "&123" => bad,
        exp_deref: "*123" => Exp::Deref(exp(123)),
        exp_record: "{ a: 123, b: 456 }" => Exp::Record(Record::from(vec![(id("a"), int(123)), (id("b"), int(456))])),
        exp_field_access: "abc.a" => Exp::FieldAccess(exp("abc"), id("a")),
        exp_null: "null" => Exp::Null;

        stm
        ---
        stm_assign: "abc = 123;" => Stm::Assign(Id::from("abc"), Exp::Int(123)),
        stm_output: "output 123;" => Stm::Output(Exp::Int(123)),
        stm_if: "if 1 == 1 { abc = 456; }" => Stm::If(bin_op(1, Op::Eq, 1), block!(box Stm::Assign(Id::from("abc"), Exp::Int(456))), None),
        stm_if_else: "if 1 == 1 { abc = 456; } else { abc = 789; }" => Stm::If(Exp::BinOp(Exp::Int(1).into_box(), Op::Eq, Exp::Int(1).into_box()), block!(box Stm::Assign(Id::from("abc"), Exp::Int(456))), Some(block!(box Stm::Assign(Id::from("abc"), Exp::Int(789))))),
        stm_while: "while 1 == 1 { abc = 456; }" => Stm::While(bin_op(1, Op::Eq, 1), block!(box Stm::Assign(Id::from("abc"), Exp::Int(456)))),
        stm_store: "*ptr = 456;" => Stm::Store(Exp::Id(Id::from("ptr")), Exp::Int(456)),
        stm_field_update: "abc.a = 123;" => Stm::FieldUpdate(Id::from("abc"), Id::from("a"), Exp::Int(123)),
        stm_indirect_field_update: "(*ptr).a = 456;" => Stm::IndirectFieldUpdate(Exp::Id(Id::from("ptr")), Id::from("a"), Exp::Int(456)),
        stm_if_else2: "if n==0 { f = 1; } else { f = n * recurse(n-1); }" => Stm::If(bin_op("n", Op::Eq, 0), block!(box Stm::Assign(Id::from("f"), Exp::Int(1))), Some(block!(box Stm::Assign(Id::from("f"), bin_op("n", Op::Mul, Exp::Call(exp("recurse"), vec![bin_op("n", Op::Sub, 1)]))))));

        fun
        ---
        fun_works: "fun(a, b) { var c; output a + b; return a; }" => fun("fun", vec!["a", "b"], vec!["c"], vec![Stm::Output(bin_op("a", Op::Add, "b"))], id("a")),
        fun_no_locals: "fun() { return 0; }" => fun("fun", vec![], vec![], vec![], int(0));

        program
        -------
        program_works: "fun() { return 0; } fun2() { return 1; }" => Program { funs: vec![fun("fun", vec![], vec![], vec![], int(0)), fun("fun2", vec![], vec![], vec![], int(1))] };
    }
}

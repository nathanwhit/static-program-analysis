use ast::{BinaryOp, Exp, ExpId, Fun, Id, Program, Record, Stm, StmId};

use crate::{adhoc, error, ParseResult, Parser, TokenKind, T};

fn id(p: &mut Parser) -> ParseResult<Id> {
    match p.bump_slice(TokenKind::Id) {
        Ok(id) => Ok(Id(id.to_string())),
        Err(e) => Err(e.wrap(adhoc("Expected identifier"))),
    }
}

fn int(p: &mut Parser) -> ParseResult<i64> {
    match p.bump_slice(TokenKind::Int) {
        Ok(int) => match int.parse::<i64>() {
            Ok(int) => Ok(int),
            Err(_) => Err(error(p.current_tok(), "Invalid integer")),
        },
        Err(e) => Err(e.wrap(adhoc("Expected integer"))),
    }
}

fn op(p: &mut Parser) -> Option<BinaryOp> {
    if let Some(op) = p.current().to_op() {
        p.bump_tok();
        Some(op)
    } else {
        None
    }
}

macro_rules! prefix_ops {
    () => {
        T![&] | T![*] | T![alloc] | T![-]
    };
}

macro_rules! postfix_ops {
    () => {
        TokenKind::LParen
    };
}

macro_rules! infix_ops {
    () => {
        infix_ops!(no overlap) | T![*] | T![-]
    };
    (no overlap) => {
        T![+] | T![/] | T![==] | T![!=] | T![<] | T![>] | T![<=] | T![>=] | T![.]
    };
}

macro_rules! ops {
    () => {
        infix_ops!(no overlap) | prefix_ops!() | postfix_ops!()
    };
}

macro_rules! all_tokens {
    () => {
        TokenKind::Var
            | TokenKind::Colon
            | TokenKind::Eq
            | TokenKind::Input
            | TokenKind::Output
            | TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::LBrace
            | TokenKind::RBrace
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::While
            | TokenKind::Return
            | TokenKind::Alloc
            | TokenKind::Ampersand
            | TokenKind::Star
            | TokenKind::Comma
            | TokenKind::Semi
            | TokenKind::EqEq
            | TokenKind::Null
            | TokenKind::Dot
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::LEq
            | TokenKind::GEq
            | TokenKind::NEq
            | TokenKind::Plus
            | TokenKind::Dash
            | TokenKind::Slash
            | TokenKind::Int
            | TokenKind::Id
            | TokenKind::Eof
    };
}

impl TokenKind {
    fn is_infix_op(self) -> bool {
        matches!(self, infix_ops!())
    }

    fn is_prefix_op(self) -> bool {
        matches!(self, prefix_ops!())
    }

    fn is_postfix_op(self) -> bool {
        matches!(self, postfix_ops!())
    }

    fn to_op(self) -> Option<BinaryOp> {
        match self {
            t if t.is_infix_op() => {
                let op = match self {
                    T![+] => BinaryOp::Add,
                    T![-] => BinaryOp::Sub,
                    T![/] => BinaryOp::Div,
                    T![*] => BinaryOp::Mul,
                    T![==] => BinaryOp::Eq,
                    T![!=] => BinaryOp::NEq,
                    T![<] => BinaryOp::Lt,
                    T![>] => BinaryOp::Gt,
                    T![<=] | T![>=] => todo!(),
                    T![.] => return None,
                    _ => unreachable!(),
                };
                Some(op)
            }
            _ => None,
        }
    }
}

// test(exp) record
// { x: 1, y: 2 }

// test(exp) record_nested
// { x: { y: 1, z: 2 }, y: 2 }
fn record(p: &mut Parser) -> ParseResult<Record> {
    p.bump(TokenKind::LBrace)?;

    let mut fields = Vec::new();
    while !p.at(TokenKind::RBrace) && !p.at_end() {
        let id = id(p)?;
        p.bump(TokenKind::Colon)?;
        let exp = exp(p)?;
        if !p.at(TokenKind::RBrace) && !p.at_end() {
            // test_err(exp) record_no_comma
            // { x: 1 y: 2 z: 3 }
            p.bump(TokenKind::Comma)?;
        }
        fields.push((id, p.alloc(exp)));
    }
    // test_err(exp) record_no_close
    // { x: 1, y: 2
    p.bump(TokenKind::RBrace)?;
    Ok(Record::from(fields))
}

pub fn exp(p: &mut Parser) -> ParseResult<Exp> {
    exp_bp(p, 0)
}

fn exp_bp(p: &mut Parser, min_bp: u8) -> ParseResult<Exp> {
    let mut lhs = match p.current() {
        // test(exp) parenthesized
        // (1)
        TokenKind::LParen => {
            p.bump(TokenKind::LParen)?;
            let exp = exp_bp(p, 0)?;
            p.bump(TokenKind::RParen)?;
            exp
        }
        T![null] => {
            p.bump(T![null])?;
            Exp::Null
        }
        T![input] => {
            p.bump(T![input])?;
            Exp::Input
        }
        // test(exp) int_exp
        // 1
        TokenKind::Int => Exp::Int(int(p)?),
        // test(exp) id_exp
        // x
        TokenKind::Id => Exp::Id(id(p)?),
        TokenKind::LBrace => {
            let record = record(p)?;
            Exp::Record(record)
        }
        t @ prefix_ops!() => {
            p.bump_tok();
            let ((), r_bp) = prefix_binding_power(t).unwrap();
            let rhs = exp_bp(p, r_bp)?;
            // test(exp) prefix_exp
            // *&x + *alloc x
            match t {
                // test(exp) ref_exp
                // &x
                T![&] => {
                    let Exp::Id(id) = rhs else {
                        // test_err(exp) ref_exp_no_id
                        // &1
                        return Err(error(p.current_tok(), "Can only take reference of id"));
                    };
                    Exp::Ref(id)
                }
                // test(exp) deref_exp
                // *x
                T![*] => Exp::Deref(p.alloc(rhs)),
                // test(exp) alloc_exp
                // alloc x
                T![alloc] => Exp::Alloc(p.alloc(rhs)),
                // test(exp) neg_exp
                // --x
                T![-] => Exp::UnaryOp(ast::UnaryOp::Neg, p.alloc(rhs)),
                _ => unreachable!(),
            }
        }
        // test_err(exp) non_exp
        // var x;
        _ => return Err(error(p.current_tok(), "Expected expression")),
    };

    loop {
        let operator = match p.current() {
            TokenKind::Eof => break,
            t @ ops!() => t,
            _ => break,
        };

        if let Some((l_bp, ())) = postfix_binding_power(operator) {
            if l_bp < min_bp {
                break;
            }
            p.bump_tok();

            lhs = match operator {
                // test(exp) call_exp
                // f(a, g(b), c.x)
                TokenKind::LParen => {
                    let mut args = Vec::new();
                    while !p.at(TokenKind::RParen) && !p.at_end() {
                        let arg = exp(p)?;
                        args.push(p.alloc(arg));
                        if p.at(TokenKind::RParen) {
                            break;
                        }
                        p.bump(TokenKind::Comma)?;
                    }
                    p.bump(TokenKind::RParen)?;
                    Exp::Call(p.alloc(lhs), args)
                }
                _ => unreachable!(),
            };
            continue;
        }

        if let Some((l_bp, r_bp)) = infix_binding_power(operator) {
            if l_bp < min_bp {
                break;
            }
            p.bump_tok();

            let rhs = exp_bp(p, r_bp)?;
            lhs = if let Some(bin_op) = operator.to_op() {
                // test(exp) bin_op_exp
                // 1 * 2 + 3 / 4 - 5 == 6

                // test(exp) bin_op_no_space
                // 2-1
                Exp::BinOp(p.alloc(lhs), bin_op, p.alloc(rhs))
            } else {
                match operator {
                    // test(exp) field_access
                    // x.y
                    T![.] => {
                        let Exp::Id(id) = rhs else {
                            return Err(error(p.current_tok(), "Expected identifier"));
                        };
                        // test(exp) nested_field_access
                        // x.y + (*alloc x).z.w
                        Exp::FieldAccess(p.alloc(lhs), id)
                    }
                    #[allow(unreachable_patterns)]
                    all_tokens!() => unreachable!(),
                }
            };
            continue;
        }

        break;
    }

    Ok(lhs)
}

fn prefix_binding_power(kind: TokenKind) -> Option<((), u8)> {
    match kind {
        T![-] => Some(((), 9)),
        T![&] => Some(((), 9)),
        T![*] => Some(((), 10)),
        T![alloc] => Some(((), 11)),
        _ => None,
    }
}

fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8)> {
    match kind {
        T![==] | T![!=] => Some((1, 2)),
        T![<] | T![>] => Some((1, 2)),
        T![<=] | T![>=] => Some((1, 2)),
        T![+] | T![-] => Some((3, 4)),
        T![*] | T![/] => Some((5, 6)),
        T![.] => Some((7, 8)),
        _ => None,
    }
}

fn postfix_binding_power(kind: TokenKind) -> Option<(u8, ())> {
    match kind {
        TokenKind::LParen => Some((12, ())),
        _ => None,
    }
}

struct FunBlock {
    locals: Vec<Id>,
    body: Vec<StmId>,
    ret: Option<ExpId>,
}

enum MaybeFunBlock {
    Fun(FunBlock),
    Block(Vec<StmId>),
}

enum BlockKind {
    Fun,
    Block,
}

impl BlockKind {
    fn is_fun(&self) -> bool {
        matches!(self, BlockKind::Fun)
    }
}

fn block_impl(p: &mut Parser, kind: BlockKind) -> ParseResult<MaybeFunBlock> {
    assert!(p.at(TokenKind::LBrace));

    let mut stms = Vec::new();

    p.bump(TokenKind::LBrace)?;

    let mut locals = Vec::new();
    let mut ret = None;

    if p.eat(T![var]) {
        if !kind.is_fun() {
            // test_err(stm) var_outside_fun
            // {
            //  var x;
            // }
            return Err(error(
                p.current_tok(),
                "Can only declare locals at top of function body",
            ));
        }
        while !p.at(T![;]) && !p.at_end() {
            let id = id(p)?;
            if !p.at(T![;]) && !p.at_end() {
                p.bump(T![,])?;
            }
            locals.push(id);
        }
        p.bump(T![;])?;
    }

    while !p.at(TokenKind::RBrace) && !p.at_end() {
        if p.at(T![return]) {
            if !kind.is_fun() {
                // test_err(stm) return_outside_fun
                // {
                //  return 1;
                // }
                return Err(error(
                    p.current_tok(),
                    "Can only return from inside a function",
                ));
            }

            p.bump_tok();
            let exp = exp(p)?;
            ret = Some(p.alloc(exp));
            p.bump(T![;])?;
            continue;
        }
        let stm = stm(p)?;
        stms.push(p.alloc(stm));
    }

    p.bump(TokenKind::RBrace)?;

    match kind {
        BlockKind::Fun => Ok(MaybeFunBlock::Fun(FunBlock {
            locals,
            body: stms,
            ret,
        })),
        BlockKind::Block => Ok(MaybeFunBlock::Block(stms)),
    }
}

// test(fun) fun_block
// f() {
//  var a;
//  return a;
// }
fn fun_block(p: &mut Parser) -> ParseResult<FunBlock> {
    let MaybeFunBlock::Fun(fun) = block_impl(p, BlockKind::Fun)? else {
        unreachable!()
    };

    Ok(fun)
}

fn block(p: &mut Parser) -> ParseResult<Vec<StmId>> {
    let MaybeFunBlock::Block(block) = block_impl(p, BlockKind::Block)? else {
        unreachable!()
    };

    Ok(block)
}

pub fn stm(p: &mut Parser) -> ParseResult<Stm> {
    Ok(match p.current() {
        TokenKind::Id => {
            // assign or field update
            match p.nth(1) {
                T![=] => {
                    // test(stm) assign
                    // x = 1;
                    let id = id(p)?;
                    p.bump(T![=])?;
                    let exp = exp(p)?;

                    // test_err(stm) assign_no_semi
                    // x = 1
                    p.bump(T![;])?;

                    Stm::Assign(id, p.alloc(exp))
                }
                T![.] => {
                    // test(stm) field_update
                    // x.y = 1;
                    let record = id(p)?;
                    p.bump(T![.])?;
                    let field = id(p)?;
                    p.bump(T![=])?;
                    let rhs = exp(p)?;
                    // test_err(stm) field_update_no_semi
                    // x.y = 1
                    p.bump(T![;])?;

                    Stm::FieldUpdate(record, field, p.alloc(rhs))
                }
                _ => {
                    // test_err(stm) ident_followed_by_unexpected
                    // x &1;
                    return Err(error(
                        p.current_tok(),
                        "Expected assignment or field update",
                    ));
                }
            }
        }
        T![output] => {
            // test(stm) output
            // output 1;
            p.bump_tok();
            let exp = exp(p)?;

            // test_err(stm) output_no_semi
            // output 1
            p.bump(T![;])?;

            Stm::Output(p.alloc(exp))
        }
        T![while] => {
            // test(stm) while
            // while 1 { a = 1; }
            p.bump_tok();
            let cond = exp(p)?;
            let body = block(p)?;

            Stm::While(p.alloc(cond), p.alloc(Stm::Block(body)))
        }
        T![if] => {
            // test(stm) if
            // if 1 { a = 1; }
            p.bump_tok();
            let cond = exp(p)?;
            let body = block(p)?;

            // test(stm) if_else
            // if 1 { a = 1; } else { a = 2; }
            let else_body = if p.eat(T![else]) {
                Some(block(p)?)
            } else {
                None
            };

            Stm::If(
                p.alloc(cond),
                p.alloc(Stm::Block(body)),
                else_body.map(|b| p.alloc(Stm::Block(b))),
            )
        }
        T![return] => {
            // test(stm) return
            // return 1;
            p.bump_tok();
            let exp = exp(p)?;

            // test_err(stm) return_no_semi
            // return 1
            p.bump(T![;])?;

            Stm::Return(p.alloc(exp))
        }
        TokenKind::LBrace => {
            // test(stm) block
            // { a = 1; }
            Stm::Block(block(p)?)
        }
        TokenKind::LParen => {
            // test(stm) indirect_field_update
            // (*x).y = 1;
            p.bump_tok();
            p.bump(T![*])?;
            let record_ptr = exp(p)?;
            p.bump(TokenKind::RParen)?;
            p.bump(T![.])?;
            let field = id(p)?;
            p.bump(T![=])?;
            let rhs = exp(p)?;
            p.bump(T![;])?;

            Stm::IndirectFieldUpdate(p.alloc(record_ptr), field, p.alloc(rhs))
        }
        T![*] => {
            // test(stm) store
            // *x = 1;
            p.bump_tok();
            let ptr = exp(p)?;
            p.bump(T![=])?;
            let rhs = exp(p)?;
            p.bump(T![;])?;

            Stm::Store(p.alloc(ptr), p.alloc(rhs))
        }
        _ => return Err(error(p.current_tok(), "Expected statement")),
    })
}

// test(fun) fun
// f(x, y) {
//   var a,b,c;
//   a = 1;
//   b = 2;
//   c = 3;
//   return a + b + c + x + y;
// }
pub fn fun(p: &mut Parser) -> ParseResult<Fun> {
    let name = id(p)?;
    p.bump(TokenKind::LParen)?;
    let mut args = Vec::new();
    while !p.at(TokenKind::RParen) && !p.at_end() {
        let param = id(p)?;
        args.push(param);
        if p.at(TokenKind::RParen) {
            break;
        }
        p.bump(TokenKind::Comma)?;
    }
    p.bump(TokenKind::RParen)?;

    let FunBlock { locals, body, ret } = fun_block(p)?;

    let Some(ret) = ret else {
        // test_err(fun) fun_no_return
        // f() {}
        return Err(error(p.current_tok(), "Expected return statement"));
    };

    Ok(Fun {
        name,
        args,
        locals,
        body,
        ret,
    })
}

// test(file) program
// f(x, y) {
//  return 0;
// }
// g(x, y) {
//  return 1;
// }
// main() {
//  output f(1, 2) + g(3, 4);
//  return 0;
// }
pub fn program(p: &mut Parser) -> ParseResult<Program> {
    let mut funs = Vec::new();
    while !p.at_end() {
        let fun = fun(p)?;
        funs.push(fun);
    }

    Ok(Program { funs })
}

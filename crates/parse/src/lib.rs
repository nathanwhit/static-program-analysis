#![allow(dead_code)]

mod grammar;
mod lex;
#[cfg(test)]
mod tests;

use std::cell::Cell;

use ast::AstCtx;
use common::{derive_common, Alloc, Allocable};
use lex::{LexerError, Token, TokenKind};
use thiserror::Error;

pub(crate) use lex::T;

#[derive(Debug, Error, Clone)]
#[error("ParseError: {kind}")]
pub struct ParseError {
    pub span: Span,
    #[source]
    pub kind: ParseErrorKind,
}

impl ParseError {
    fn wrap(self, kind: ParseErrorKind) -> Self {
        Self {
            span: self.span,
            kind: ParseErrorKind::Wrap(Box::new(self.clone()), Box::new(kind)),
        }
    }
}

impl From<LexerError> for ParseError {
    fn from(value: LexerError) -> Self {
        Self {
            span: value.span,
            kind: value.into(),
        }
    }
}

pub type ParseResult<T, E = ParseError> = Result<T, E>;

#[derive(Debug, Error, Clone)]
pub enum ParseErrorKind {
    #[error("Unexpected end of input")]
    UnexpectedEof,

    #[error("{0}")]
    AdHoc(String),

    #[error("Lexer error: {0}")]
    Lexer(#[from] LexerError),

    #[error("{1}: {0}")]
    Wrap(Box<ParseError>, Box<ParseErrorKind>),
}

#[derive(Copy)]
#[derive_common]
pub struct Span {
    /// inclusive start of span
    pub start: usize,
    /// exclusive end of span
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn into_range(self) -> std::ops::Range<usize> {
        self.into()
    }
}

impl From<Span> for std::ops::Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(range: std::ops::Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

#[derive_common]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { span, value }
    }
}

#[derive(Clone)]
pub struct Parser<'i> {
    input: &'i str,
    tokens: Vec<lex::Token>,
    pos: usize,
    ast_ctx: AstCtx,
    steps: Cell<u32>,
}

const STEP_LIMIT: u32 = 100_000;

impl<'i> Parser<'i> {
    pub fn new(input: &'i str) -> ParseResult<Self> {
        let tokens = lex::lex(input)?;
        Ok(Self {
            input,
            tokens,
            pos: 0,
            ast_ctx: AstCtx::new(),
            steps: Cell::new(0),
        })
    }

    #[inline]
    pub fn current(&self) -> TokenKind {
        self.current_tok().kind
    }

    pub fn current_tok(&self) -> Token {
        self.tokens[self.pos]
    }

    pub fn bump_current(&mut self) -> TokenKind {
        let current = self.current();
        self.bump_tok();
        current
    }

    pub fn next_tok(&mut self) -> Token {
        self.tokens[self.pos + 1]
    }

    fn bump_tok(&mut self) {
        self.pos += 1;
        self.steps.set(0);
    }

    pub fn nth_tok(&self, n: usize) -> Token {
        let steps = self.steps.get();
        if steps > STEP_LIMIT {
            panic!("Parser seems stuck");
        }

        self.steps.set(steps + 1);
        self.tokens[(self.pos + n).min(self.tokens.len() - 1)]
    }

    pub fn nth(&self, n: usize) -> TokenKind {
        self.nth_tok(n).kind
    }

    pub fn bump(&mut self, kind: TokenKind) -> ParseResult<()> {
        self.bump_slice(kind).map(|_| ())
    }

    pub fn bump_slice(&mut self, kind: TokenKind) -> ParseResult<&'i str> {
        if self.at(kind) {
            let token = self.current_tok();
            self.bump_tok();
            Ok(self.slice(token))
        } else {
            Err(ParseError {
                span: self.current_tok().span,
                kind: ParseErrorKind::AdHoc(format!(
                    "Expected {:?}, found {:?}",
                    kind,
                    self.current_tok().kind
                )),
            })
        }
    }

    pub fn at(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    pub fn at_end(&self) -> bool {
        self.at(TokenKind::Eof)
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.bump_tok();
            true
        } else {
            false
        }
    }

    pub fn slice(&self, token: Token) -> &'i str {
        &self.input[token.span.start..token.span.end]
    }

    pub fn alloc<T: Allocable>(&mut self, value: T) -> T::Idx
    where
        AstCtx: Alloc<T>,
    {
        self.ast_ctx.alloc(value)
    }

    pub fn finish(self) -> AstCtx {
        self.ast_ctx
    }
}

fn error(token: Token, msg: impl AsRef<str>) -> ParseError {
    ParseError {
        span: token.span,
        kind: ParseErrorKind::AdHoc(msg.as_ref().to_string()),
    }
}

fn adhoc(msg: impl AsRef<str>) -> ParseErrorKind {
    ParseErrorKind::AdHoc(msg.as_ref().to_string())
}

pub fn parse(input: &str) -> ParseResult<(AstCtx, ast::Program)> {
    let mut parser = Parser::new(input)?;
    let ast = grammar::program(&mut parser)?;
    Ok((parser.finish(), ast))
}

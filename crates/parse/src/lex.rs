use crate::Span;
use logos::Logos;
use std::num::ParseIntError;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
#[error("{kind}")]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub span: Span,
}

#[derive(Error, Debug, Clone, PartialEq, Eq, Default)]
pub enum LexerErrorKind {
    #[error("invalid integer: {0}")]
    InvalidInt(#[from] ParseIntError),

    #[error("unexpected character")]
    #[default]
    UnexpectedChar,
}

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
#[logos(error = LexerErrorKind)]
#[logos(skip r"[ \t\n\r\f]+")]
pub enum TokenKind {
    #[token("var")]
    Var,

    #[token(":")]
    Colon,

    #[token("=")]
    Eq,

    #[token("input")]
    Input,

    #[token("output")]
    Output,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("while")]
    While,

    #[token("return")]
    Return,

    #[token("alloc")]
    Alloc,

    #[token("&")]
    Ampersand,

    #[token("*")]
    Star,

    #[token(",")]
    Comma,

    #[token(";")]
    Semi,

    #[token("==")]
    EqEq,

    #[token("null")]
    Null,

    #[token(".")]
    Dot,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    #[token("<=")]
    LEq,

    #[token(">=")]
    GEq,

    #[token("!=")]
    NEq,

    #[token("+")]
    Plus,

    #[token("-")]
    Dash,

    #[token("/")]
    Slash,

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().map(|_| ()))]
    Int,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Id,

    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub fn lex(input: &str) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    for (kind, span) in TokenKind::lexer(input).spanned() {
        let span = Span::from(span);
        let kind = match kind {
            Ok(kind) => kind,
            Err(err) => {
                return Err(LexerError { kind: err, span });
            }
        };
        tokens.push(Token { kind, span });
    }
    tokens.push(Token {
        kind: TokenKind::Eof,
        span: Span::new(input.len(), input.len()),
    });

    Ok(tokens)
}

macro_rules! T {
    (,) => {
        TokenKind::Comma
    };
    (:) => {
        TokenKind::Colon
    };
    (;) => {
        TokenKind::Semi
    };
    (+) => {
        TokenKind::Plus
    };
    (-) => {
        TokenKind::Dash
    };
    (/) => {
        TokenKind::Slash
    };
    (*) => {
        TokenKind::Star
    };
    (==) => {
        TokenKind::EqEq
    };
    (!=) => {
        TokenKind::NEq
    };
    (<) => {
        TokenKind::Lt
    };
    (>) => {
        TokenKind::Gt
    };
    (<=) => {
        TokenKind::LEq
    };
    (>=) => {
        TokenKind::GEq
    };
    (=) => {
        TokenKind::Eq
    };
    (&) => {
        TokenKind::Ampersand
    };
    (.) => {
        TokenKind::Dot
    };
    (null) => {
        TokenKind::Null
    };
    (var) => {
        TokenKind::Var
    };
    (input) => {
        TokenKind::Input
    };
    (output) => {
        TokenKind::Output
    };
    (if) => {
        TokenKind::If
    };
    (else) => {
        TokenKind::Else
    };
    (while) => {
        TokenKind::While
    };
    (return) => {
        TokenKind::Return
    };
    (alloc) => {
        TokenKind::Alloc
    };
    (null) => {
        TokenKind::Null
    };
    (int) => {
        TokenKind::Int
    };
    (id) => {
        TokenKind::Id
    };
}

pub(crate) use T;

mod sourcegen_inline_tests;

use std::{
    fs,
    path::{Path, PathBuf},
};

use ast::{AstCtx, Exp, Fun, Program, Stm};
use insta::{assert_display_snapshot, assert_ron_snapshot};

use crate::{ParseResult, Parser};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
enum EntryPoint {
    SourceFile,
    Exp,
    Stm,
    Fun,
}

enum Ast {
    Exp(Exp),
    Stm(Stm),
    Fun(Fun),
    Program(Program),
}

impl EntryPoint {
    pub fn parse<'i>(&self, input: &'i str) -> (ParseResult<Ast>, AstCtx) {
        let parse: fn(&mut Parser<'_>) -> ParseResult<Ast> = match self {
            EntryPoint::Exp => |p| crate::grammar::exp(p).map(Ast::Exp),
            EntryPoint::Stm => |p| crate::grammar::stm(p).map(Ast::Stm),
            EntryPoint::SourceFile => |p| crate::grammar::program(p).map(Ast::Program),
            EntryPoint::Fun => |p| crate::grammar::fun(p).map(Ast::Fun),
        };
        let mut p = Parser::new(input).unwrap();
        let res = parse(&mut p);
        if res.is_ok() {
            assert!(
                p.at_end(),
                "parser did not consume entire input. on {:?}",
                p.current_tok()
            );
        }
        (res, p.finish())
    }
}

// Uncomment if we add any of these tests
// #[test]
// fn lex_ok() {
//     insta::glob!("../test_data", "lexer/ok/*.tip", |path| {
//         let case = TestCase::read(path);
//         let actual = lex(&case.text);
//         insta::with_settings!({description => &case.text, omit_expression => true }, { assert_snapshot!(actual) });
//     });
// }

// Uncomment if we add any of these tests
// #[test]
// fn lex_err() {
//     for case in TestCase::list("lexer/err") {
//         let actual = lex(&case.text);
//         assert_snapshot!(actual)
//     }
// }

macro_rules! test_glob {
    (@helper $actual: ident, $ctx: ident, $case: ident) => {
        {
            let wrapped = common::serde::SerializeWrapper::new(&$actual, &$ctx);
            insta::with_settings!({description => &$case.text, omit_expression => true, sort_maps => true }, { assert_ron_snapshot!(wrapped) });
        }
    };
    (ok $glob: expr) => {
        insta::glob!("../test_data", &format!("parse/{}/*.tip", $glob), |path| {
            let case = TestCase::read(path);
            eprintln!("running test: {}", case.tip.display());
            let (actual, ctx) = case.entry.parse(&case.text);
            let actual = actual.unwrap();
            match actual {
                Ast::Exp(actual) => test_glob!(@helper actual, ctx, case),
                Ast::Stm(actual) => test_glob!(@helper actual, ctx, case),
                Ast::Fun(actual) => test_glob!(@helper actual, ctx, case),
                Ast::Program(actual) => test_glob!(@helper actual, ctx, case),
            }
        });
    };
    (err $glob: expr) => {
        insta::glob!("../test_data", &format!("parse/{}/*.tip", $glob), |path| {
            let case = TestCase::read(path);
            eprintln!("running test: {}", case.tip.display());
            let (actual, _ctx) = case.entry.parse(&case.text);
            let actual = actual.map(|_| ()).unwrap_err();

            insta::with_settings!({description => &case.text, omit_expression => true, sort_maps => true}, { assert_display_snapshot!(actual) });
        });
    };
}

// #[test]
// fn parse_ok() {
//     test_glob!(ok "ok/file");
// }

// Uncomment if we add any of these tests
// #[test]
// fn parse_err() {
//     for case in TestCase::list("parser/err") {
//         let (actual, errors) = parse(EntryPoint::SourceFile, &case.text);
//         assert!(
//             errors,
//             "no errors in an ERR file {}:\n{actual}",
//             case.tip.display()
//         );
//         assert_snapshot!(actual)
//     }
// }

#[test]
fn parse_inline_ok() {
    test_glob!(ok "inline/ok/exp");
    test_glob!(ok "inline/ok/stm");
    test_glob!(ok "inline/ok/fun");
    test_glob!(ok "inline/ok/file");
}

#[test]
fn parse_inline_err() {
    test_glob!(err "inline/err/exp");
    test_glob!(err "inline/err/stm");
    test_glob!(err "inline/err/fun");
    // test_glob!(err "inline/err/file");
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TestCase {
    tip: PathBuf,
    entry: EntryPoint,
    text: String,
}

fn entry_point_from_str(s: &str) -> EntryPoint {
    match s {
        "exp" => EntryPoint::Exp.into(),
        "file" => EntryPoint::SourceFile.into(),
        "stm" => EntryPoint::Stm.into(),
        "fun" => EntryPoint::Fun.into(),
        "lexer" => todo!(),
        _ => panic!("unknown entry point {s}"),
    }
}

impl TestCase {
    fn read(path: &Path) -> TestCase {
        if path
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .file_name()
            .unwrap()
            == "lexer"
        {
            todo!()
        }
        let entry = entry_point_from_str(
            path.parent()
                .unwrap()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap(),
        );
        if path.extension().unwrap_or_default() == "tip" {
            let tip = path.into();
            let text = fs::read_to_string(&tip).unwrap();
            TestCase { tip, text, entry }
        } else {
            panic!("unknown file extension {}", path.display());
        }
    }

    #[allow(unused)]
    fn list(path: &str) -> Vec<TestCase> {
        let crate_root_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let test_data_dir = crate_root_dir.join("test_data");
        let dir = test_data_dir.join(path);

        let mut res = Vec::new();
        let read_dir = fs::read_dir(&dir)
            .unwrap_or_else(|err| panic!("can't `read_dir` {}: {err}", dir.display()));
        for file in read_dir {
            let file = file.unwrap();
            let path = file.path();
            if path.is_dir() {
                let entry = entry_point_from_str(path.file_name().unwrap().to_str().unwrap());
                for case in Self::list(path.to_str().unwrap()) {
                    res.push(TestCase { entry, ..case })
                }
                continue;
            }
            if path.extension().unwrap_or_default() == "tip" {
                let tip = path;
                let text = fs::read_to_string(&tip).unwrap();
                res.push(TestCase {
                    tip,
                    text,
                    entry: EntryPoint::SourceFile.into(),
                });
            }
        }
        res.sort();
        res
    }
}

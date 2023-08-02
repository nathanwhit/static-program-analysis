use ariadne::{sources, Label, Report, ReportKind};
use clap::Parser;
use std::path::PathBuf;
use tracing_subscriber::{prelude::__tracing_subscriber_SubscriberExt, Registry};
use tracing_tree::HierarchicalLayer;

#[derive(Parser)]
struct Cli {
    file: PathBuf,
}

fn main() -> Result<(), ()> {
    let cli = Cli::parse();
    let _guard = if std::env::var("RUST_LOG").is_ok() {
        let _reg = Registry::default().with(HierarchicalLayer::new(2));
        Some(tracing::subscriber::set_default(_reg))
    } else {
        None
    };

    let file = std::fs::read_to_string(cli.file.clone()).unwrap();
    let path_str = cli.file.to_str().unwrap().to_owned();
    let (ctx, ast) = match parse::parse(&file) {
        Ok(ast) => ast,
        Err(err) => {
            let cache = sources(vec![(path_str.clone(), file.as_str())]);
            Report::build::<&str>(ReportKind::Error, path_str.as_str(), err.span.start)
                .with_label(
                    Label::new((path_str.clone(), err.span.into_range()))
                        .with_message(err.to_string()),
                )
                .finish()
                .print(cache)
                .unwrap();
            return Err(());
        }
    };
    let _output = interpret::program(&ctx, &ast).unwrap();

    Ok(())
}

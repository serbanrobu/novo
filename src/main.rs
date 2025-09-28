use im::HashMap;
use indexmap::IndexMap;
use novo::{
    compile::{interpret_source_program, interpret_source_program_line},
    ir::{Context, Diagnostic, Output},
};
use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
    sync::Arc,
};

use clap::Parser;
use clap_cargo::style::CLAP_STYLING;
use miette::{IntoDiagnostic, Result, miette};
use novo::{db::DatabaseImpl, ir::SourceProgram};
use rustyline::{DefaultEditor, error::ReadlineError};
use salsa::Setter;

#[derive(Debug, Parser)]
#[command(about, styles = CLAP_STYLING, version)]
enum Cli {
    /// Evaluate a script from the command line
    Eval { code: String },
    /// Start an interactive Read-Eval-Print Loop (REPL) for Novo
    Repl,
    /// Run a Novo program
    Run { file: PathBuf },
}

fn main() -> Result<()> {
    miette::set_panic_hook();

    match Cli::parse() {
        Cli::Eval { code } => eval(code),
        Cli::Repl => repl(),
        Cli::Run { file } => {
            let code = fs::read_to_string(file).into_diagnostic()?;
            eval(code)
        }
    }
}

fn eval(code: String) -> Result<()> {
    let db = &DatabaseImpl::new();
    let source = Arc::new(code);
    let source_program = SourceProgram::new(db, source.clone());
    let context = Context::new(db, HashMap::new());
    interpret_source_program(db, source_program, context);

    let diagnostics =
        interpret_source_program::accumulated::<Diagnostic>(db, source_program, context);

    print_diagnostics(diagnostics, &source)?;
    let outputs = interpret_source_program::accumulated::<Output>(db, source_program, context);
    print_outputs(&outputs)?;
    Ok(())
}

fn repl() -> Result<()> {
    let db = &mut DatabaseImpl::new();
    let context = Context::new(db, HashMap::new());
    let source_program = SourceProgram::new(db, "".to_owned().into());
    let mut rl = DefaultEditor::new().into_diagnostic()?;

    println!(
        "{} {}\nExit using CTRL+D or CTRL+C.",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
    );

    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("{% ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).into_diagnostic()?;
                handle_line(db, source_program, context, line)?;
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    #[cfg(feature = "with-file-history")]
    let _ = rl.save_history("history.txt");

    Ok(())
}

fn handle_line(
    db: &mut DatabaseImpl,
    source_program: SourceProgram,
    context: Context,
    line: String,
) -> Result<()> {
    let source = Arc::new(line);
    source_program.set_text(db).to(source.clone());
    interpret_source_program_line(db, source_program, context);
    let diagnostics =
        interpret_source_program_line::accumulated::<Diagnostic>(db, source_program, context);
    print_diagnostics(diagnostics, &source)?;
    let outputs = interpret_source_program_line::accumulated::<Output>(db, source_program, context);
    print_outputs(&outputs)?;

    let scope = context
        .environment(db)
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .chain(outputs.into_iter().cloned().map(|o| (o.ident, o.value)))
        .collect();

    context.set_environment(db).to(scope);
    Ok(())
}

fn print_diagnostics<'db>(
    diagnostics: impl IntoIterator<Item = &'db Diagnostic>,
    source: &Arc<String>,
) -> Result<()> {
    let mut handle = io::stderr().lock();

    for diagnostic in diagnostics {
        writeln!(
            &mut handle,
            "{:?}",
            miette!(diagnostic.clone()).with_source_code(source.clone())
        )
        .into_diagnostic()?;
    }

    Ok(())
}

fn print_outputs(outputs: &[&Output]) -> Result<()> {
    let mut handle = io::stdout().lock();

    for (x, v) in outputs
        .iter()
        .map(|o| (&o.ident, &o.value))
        .collect::<IndexMap<_, _>>()
    {
        writeln!(&mut handle, "{x} = {v}").into_diagnostic()?;
    }

    Ok(())
}

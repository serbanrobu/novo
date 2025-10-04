use crate::{
    interpreter::interpret_program,
    ir::{Context, Diagnostic, Output, Program, SourceProgram},
    parser::{parse_program, parse_program_line},
    type_check::type_check_program,
};

#[salsa::tracked]
pub fn compile<'db>(
    db: &'db dyn salsa::Database,
    source_program: SourceProgram,
    context: Context,
) -> Program<'db> {
    let program = parse_program(db, source_program);
    type_check_program(db, program, context);
    program
}

#[salsa::tracked]
pub fn compile_line<'db>(
    db: &'db dyn salsa::Database,
    source_program: SourceProgram,
    context: Context,
) -> Program<'db> {
    let program = parse_program_line(db, source_program);
    type_check_program(db, program, context);
    program
}

fn interpret<'db>(
    db: &'db dyn salsa::Database,
    program: Program<'db>,
    context: Context,
) -> Result<Vec<&'db Output>, Vec<&'db Diagnostic>> {
    interpret_program(db, program, context);
    let diagnostics: Vec<&Diagnostic> = interpret_program::accumulated(db, program, context);

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let outputs: Vec<&Output> = interpret_program::accumulated(db, program, context);
    Ok(outputs)
}

pub fn compile_and_interpret(
    db: &dyn salsa::Database,
    source_program: SourceProgram,
    context: Context,
) -> Result<Vec<&Output>, Vec<&Diagnostic>> {
    let program = compile(db, source_program, context);
    let diagnostics: Vec<&Diagnostic> = compile::accumulated(db, source_program, context);

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    interpret(db, program, context)
}

pub fn compile_and_interpret_line(
    db: &dyn salsa::Database,
    source_program: SourceProgram,
    context: Context,
) -> Result<Vec<&Output>, Vec<&Diagnostic>> {
    let program = compile_line(db, source_program, context);
    let diagnostics: Vec<&Diagnostic> = compile_line::accumulated(db, source_program, context);

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    interpret(db, program, context)
}

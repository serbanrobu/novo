use crate::{
    interpreter::interpret_program,
    ir::{Context, SourceProgram},
    parser::{parse_program, parse_program_line},
    type_check::type_check_program,
};

#[salsa::tracked]
pub fn interpret_source_program<'db>(
    db: &'db dyn salsa::Database,
    source_program: SourceProgram,
    context: Context,
) {
    let program = parse_program(db, source_program);
    #[cfg(feature = "type-check")]
    type_check_program(db, program, context);
    interpret_program(db, program, context);
}

#[salsa::tracked]
pub fn interpret_source_program_line<'db>(
    db: &'db dyn salsa::Database,
    source_program: SourceProgram,
    context: Context,
) {
    let program = parse_program_line(db, source_program);
    #[cfg(feature = "type-check")]
    type_check_program(db, program, context);
    interpret_program(db, program, context);
}

#[salsa::tracked]
pub fn type_check_source_program<'db>(
    db: &'db dyn salsa::Database,
    source_program: SourceProgram,
    context: Context,
) {
    let program = parse_program(db, source_program);
    type_check_program(db, program, context);
}

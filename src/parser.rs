use std::{convert::Infallible, fmt, ops::Range};

use lalrpop_util::ParseError;
use miette::ByteOffset;
use thiserror::Error;

use crate::{
    grammar::ProgramParser,
    ir::{Program, SourceProgram},
    lexer::Lexer,
    token::{LexicalError, Token},
};

#[derive(Clone, Debug, Error, PartialEq, miette::Diagnostic)]
pub enum ParsingError {
    #[error("invalid token")]
    #[diagnostic(code(novo::ParsingError::InvalidToken))]
    InvalidToken {
        #[label]
        offset: ByteOffset,
    },
    #[error("unrecognized EOF")]
    #[diagnostic(code(novo::ParsingError::UnrecognizedEof))]
    UnrecognizedEof {
        expected: Vec<String>,
        #[label("{}", ExpectedOneOf { expected })]
        offset: ByteOffset,
    },
    #[error("unrecognized token: `{token}`")]
    #[diagnostic(code(novo::ParsingError::UnrecognizedToken))]
    UnrecognizedToken {
        expected: Vec<String>,
        #[label("{}", ExpectedOneOf { expected })]
        span: Range<ByteOffset>,
        token: Token,
    },
    #[error("extra token: `{token}`")]
    #[diagnostic(code(novo::ParsingError::ExtraToken))]
    ExtraToken {
        #[label]
        span: Range<ByteOffset>,
        token: Token,
    },
    #[error(transparent)]
    #[diagnostic(transparent)]
    LexicalError(#[from] LexicalError),
}

impl From<ParseError<usize, Result<Token, LexicalError>, Infallible>> for ParsingError {
    fn from(parse_error: ParseError<usize, Result<Token, LexicalError>, Infallible>) -> Self {
        match parse_error {
            ParseError::InvalidToken { location } => {
                ParsingError::InvalidToken { offset: location }
            }
            ParseError::UnrecognizedEof { location, expected } => ParsingError::UnrecognizedEof {
                expected,
                offset: location,
            },
            ParseError::UnrecognizedToken {
                token: (start, Ok(token), end),
                expected,
            } => ParsingError::UnrecognizedToken {
                expected,
                span: start..end,
                token,
            },
            ParseError::ExtraToken {
                token: (start, Ok(token), end),
            } => ParsingError::ExtraToken {
                span: start..end,
                token,
            },
            ParseError::UnrecognizedToken {
                token: (_, Err(error), _),
                ..
            }
            | ParseError::ExtraToken {
                token: (_, Err(error), _),
            } => ParsingError::LexicalError(error),
        }
    }
}

struct ExpectedOneOf<'a> {
    expected: &'a [String],
}

impl<'a> fmt::Display for ExpectedOneOf<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some((first, rest)) = self.expected.split_first() else {
            return Ok(());
        };

        write!(f, "Expected one of {}", first)?;

        let Some((last, rest)) = rest.split_last() else {
            return Ok(());
        };

        for e in rest {
            write!(f, ", {}", e)?;
        }

        write!(f, " or {}", last)
    }
}

#[salsa::tracked]
pub fn parse_program<'db>(
    db: &'db dyn salsa::Database,
    source_program: SourceProgram,
) -> Program<'db> {
    let source = source_program.text(db);
    let mut lexer = Lexer::new(source);
    lexer.skip_text();
    let parser = ProgramParser::new();

    parser
        .parse(db, lexer)
        .expect("should recover from parsing errors")
}

#[salsa::tracked]
pub fn parse_program_line<'db>(
    db: &'db dyn salsa::Database,
    source_program: SourceProgram,
) -> Program<'db> {
    let source = source_program.text(db);
    let lexer = Lexer::new(source);
    let parser = ProgramParser::new();

    parser
        .parse(db, lexer)
        .expect("should recover from parsing errors")
}

#[cfg(test)]
mod tests {
    use crate::{
        db::DatabaseImpl, error::Error, expr::Expr, ir::Diagnostic, op::BinOp, stmt::Stmt,
    };

    use super::*;

    #[test]
    fn test_parse_set_statement() {
        let db = &DatabaseImpl::new();

        let source_program = SourceProgram::new(
            db,
            "the begining {% set x %}{% = {##} 42 %} the end"
                .to_owned()
                .into(),
        );

        let program = parse_program(db, source_program);
        assert!(parse_program::accumulated::<Diagnostic>(db, source_program).is_empty());
        assert_eq!(program.stmts(db).len(), 1);

        match &program.stmts(db)[0] {
            Stmt::Set { ident, expr } => {
                assert_eq!(ident.text(db), "x");
                assert!(matches!(expr, Expr::LitNumber { inner, .. } if *inner == 42.0));
            }
            _ => panic!("expected set statement"),
        }
    }

    #[test]
    fn test_parse_if_else() {
        let db = &DatabaseImpl::new();

        let source_program = SourceProgram::new(
            db,
            "{% if 1 %} then {% set x = 2 else {# ; #} set x = 3 %}{% endif"
                .to_owned()
                .into(),
        );

        let program = parse_program(db, source_program);
        assert!(parse_program::accumulated::<Diagnostic>(db, source_program).is_empty());
        assert_eq!(program.stmts(db).len(), 1);

        match &program.stmts(db)[0] {
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                assert!(matches!(cond, Expr::LitNumber { inner, .. } if *inner == 1.0));
                assert_eq!(then_branch.len(), 1);
                assert_eq!(else_branch.as_ref().unwrap().len(), 1);
            }
            _ => panic!("expected if statement"),
        }
    }

    #[test]
    fn test_parse_expression_binary_ops() {
        let db = &DatabaseImpl::new();

        let source_program =
            SourceProgram::new(db, "some math {% set x = 1 + 2 * 3".to_owned().into());

        let program = parse_program(db, source_program);
        assert!(parse_program::accumulated::<Diagnostic>(db, source_program).is_empty());

        match &program.stmts(db)[0] {
            Stmt::Set { expr, .. } => match expr {
                Expr::Binary {
                    op: BinOp::Add,
                    right,
                    ..
                } => match right.as_ref() {
                    Expr::Binary { op: BinOp::Mul, .. } => {}
                    _ => panic!("expected a number literal"),
                },
                _ => panic!("expected binary add expr"),
            },
            _ => panic!("expected set statement"),
        }
    }

    #[test]
    fn test_parse_function_call() {
        let db = &DatabaseImpl::new();

        let source_program = SourceProgram::new(
            db,
            "{% set y = foo({# first #}1, {# second #} 2)"
                .to_owned()
                .into(),
        );

        let program = parse_program(db, source_program);
        assert!(parse_program::accumulated::<Diagnostic>(db, source_program).is_empty());

        match &program.stmts(db)[0] {
            Stmt::Set { expr, .. } => match expr {
                Expr::Call { func, args, .. } => {
                    assert_eq!(args.len(), 2);
                    assert!(matches!(**func, Expr::Variable { .. }));
                }
                _ => panic!("expected call expression"),
            },
            _ => panic!("expected set statement"),
        }
    }

    #[test]
    fn test_parse_string_concat() {
        let db = &DatabaseImpl::new();
        let source_program = SourceProgram::new(db, "{% set s = 'foo' ~ 'bar'".to_owned().into());
        let program = parse_program(db, source_program);
        assert!(parse_program::accumulated::<Diagnostic>(db, source_program).is_empty());

        match &program.stmts(db)[0] {
            Stmt::Set { expr, .. } => match expr {
                Expr::Binary {
                    op: BinOp::Concat, ..
                } => {}
                _ => panic!("expected concat expr"),
            },
            _ => panic!("expected set statement"),
        }
    }

    #[test]
    fn test_parse_error_recovery() {
        let db = &DatabaseImpl::new();

        let source_program =
            SourceProgram::new(db, "{% if @ set x == 1 set y = 0 %}".to_owned().into());

        parse_program(db, source_program);

        let diagnostics = parse_program::accumulated::<Diagnostic>(db, source_program)
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        assert_eq!(
            diagnostics,
            [
                Diagnostic(Error::ParsingError(ParsingError::LexicalError(
                    LexicalError::InvalidToken { span: 6..7 }
                ))),
                Diagnostic(Error::ParsingError(ParsingError::UnrecognizedToken {
                    expected: vec!["\"=\"".to_owned()],
                    span: 14..16,
                    token: Token::EqEq,
                })),
                Diagnostic(Error::ParsingError(ParsingError::UnrecognizedEof {
                    expected: ["\"else\"", "\"elseif\"", "\"endif\"", "\"if\"", "\"set\""]
                        .map(ToOwned::to_owned)
                        .to_vec(),
                    offset: 28
                }))
            ]
        );
    }
}

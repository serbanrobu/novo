use lalrpop_util::lalrpop_mod;

pub mod compile;
pub mod db;
pub mod error;
pub mod expr;
pub mod ir;
pub mod lexer;
pub mod op;
pub mod parser;
pub mod stmt;
pub mod token;
pub mod r#type;
pub mod type_check;

lalrpop_mod!(pub grammar);

use lalrpop_util::lalrpop_mod;

pub mod compile;
pub mod db;
pub mod error;
pub mod expr;
pub mod interpreter;
pub mod ir;
pub mod lexer;
pub mod op;
pub mod parser;
pub mod server;
pub mod stmt;
pub mod token;
pub mod r#type;
pub mod type_check;
pub mod value;

lalrpop_mod!(pub grammar);

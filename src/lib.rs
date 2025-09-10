use lalrpop_util::lalrpop_mod;

pub mod db;
pub mod ir;
pub mod lexer;
pub mod op;
pub mod parser;
pub mod token;

lalrpop_mod!(pub grammar);

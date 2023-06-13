use std::{fs, io::Write};
use std::str::Split;

static LIST_EXPRS: &'static [&'static str] = &[
    "Binary   / <T,U> / left: T, operator: Token, right: U",
    "Grouping / <T>   / expression: T",
    "Literal  / <T>   / value: T",
    "Unary    / <T>   / operator: Token, right: T" 
];

pub struct GenerateAst {}

impl GenerateAst {
    pub fn main(output_dir: &str) {
        let _ = Self::define_ast(output_dir, "Expr", LIST_EXPRS);
    }

    fn define_ast(output_dir: &str, base_name: &str, exprs: &[&str]) -> std::io::Result<()> {
        let path: String = output_dir.to_owned() + "/" + base_name + ".rs";
        let mut file: fs::File = fs::File::create(path)?;
        file.write_all(b"use crate::token::Token;\n\n")?;
        for expr in exprs.iter() {
            Self::define_type(&mut file, expr);
        }
        Ok(())
    }

    fn define_type(writer: &mut fs::File, expr: &str) {
        let (mut struct_name, fields) = expr.split_once("/").unwrap();
        struct_name = struct_name.trim();
        let (mut generic, fields) = fields.split_once("/").unwrap();
        generic = generic.trim();
        Self::define_struct(writer, struct_name, generic, fields);
        let _ = writer.write_all(b"\n");
        Self::impl_struct(writer, struct_name, generic, fields);
    }

    fn define_struct(writer: &mut fs::File, struct_name: &str, generic: &str, fields: &str) {
        let field_list: Split<&str> = fields.split(",");
        let first_line: String = "pub struct ".to_owned() + struct_name + generic + " {\n";
        let _ = writer.write_all(first_line.as_bytes());
        for field in field_list {
            let field_line: String = "  pub ".to_owned() + field.trim() + ",\n";
            let _ = writer.write_all(field_line.as_bytes()); 
        }
        let _ = writer.write_all(b"}\n");
    }

    fn impl_struct(writer: &mut fs::File, struct_name: &str, generic: &str, fields: &str) {
        let field_list: Split<&str> = fields.split(",");
        let first_line: String = "impl ".to_owned() + generic + " " + struct_name + generic + " {\n";
        let _ = writer.write_all(first_line.as_bytes());
        let fn_line = "  pub fn new(".to_owned() + fields + ") -> " + struct_name + generic + " {\n";
        let _ = writer.write_all(fn_line.as_bytes());
        let struct_line = "    ".to_owned() + struct_name + " {\n";
        let _ = writer.write_all(struct_line.as_bytes());
        for field in field_list {
            let (mut field_name, _generic) = field.split_once(":").unwrap();
            field_name = field_name.trim();
            let field_line: String = "      ".to_owned() + field_name + ": " + field_name + ",\n";
            let _ = writer.write_all(field_line.as_bytes()); 
        }
        let _ = writer.write_all(b"    }\n  }\n}\n");
    }
}

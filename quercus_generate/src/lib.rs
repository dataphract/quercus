use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    str::FromStr,
};

use tree_sitter_cli as cli;

use quercus_dsl as dsl;

/// Generate a Tree-sitter language definition.
///
/// A public item `fn language() -> tree_sitter::Language;` will be written to `filename` in the
/// build script output directory.
pub fn generate<P: AsRef<Path>>(dsl: &dsl::Grammar, filename: P) {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    let grammar_json = serde_json::to_string(dsl).unwrap();
    let (name, c_src) = cli::generate::generate_parser_for_grammar(&grammar_json).unwrap();

    let src_dir = PathBuf::from_str(&out_dir).unwrap();
    let src_file_path = src_dir.to_path_buf().join("parser.c");

    let mut src_file = File::create(&src_file_path).unwrap();
    src_file.write_all(c_src.as_bytes()).unwrap();
    drop(src_file);

    let include_dir_path = src_dir.join("tree_sitter");
    std::fs::create_dir_all(&include_dir_path).unwrap();
    let mut header_file = File::create(&include_dir_path.join("parser.h")).unwrap();
    header_file
        .write_all(tree_sitter::PARSER_HEADER.as_bytes())
        .unwrap();
    drop(header_file);

    cc::Build::new()
        .include(&src_dir)
        .file(&src_file_path)
        .compile("grammar");

    let rust_src = format!(
        r#"
extern "C" {{ fn tree_sitter_{name}() -> tree_sitter::Language; }}
pub fn language() -> tree_sitter::Language {{ unsafe {{ tree_sitter_{name}() }} }}
"#
    );

    let mut rust_file = File::create(src_dir.join(filename)).unwrap();
    rust_file.write_all(rust_src.as_bytes()).unwrap();
}

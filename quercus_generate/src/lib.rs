use std::{fs::File, io::Write, path::Path};

use tree_sitter_cli as cli;

use quercus_dsl as dsl;

pub fn generate<P: AsRef<Path>>(dsl: &dsl::Grammar, out_dir: P) {
    let grammar_json = serde_json::to_string(dsl).unwrap();
    let (name, c_src) = cli::generate::generate_parser_for_grammar(&grammar_json).unwrap();

    let src_dir = out_dir.as_ref();
    let src_file_path = src_dir.to_path_buf().join("parser.c");

    let mut src_file = File::create(&src_file_path).unwrap();
    src_file.write_all(c_src.as_bytes()).unwrap();
    drop(src_file);

    let include_dir_path = src_dir.join("tree_sitter");
    std::fs::create_dir(&include_dir_path).unwrap();
    let mut header_file = File::create(&include_dir_path.join("parser.h")).unwrap();
    header_file
        .write_all(tree_sitter::PARSER_HEADER.as_bytes())
        .unwrap();
    drop(header_file);

    println!("source written to {src_file_path:?}");

    cc::Build::new()
        .include(&src_dir)
        .file(&src_file_path)
        .compile("grammar");
}

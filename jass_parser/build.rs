use lalrpop;

fn main() {
    lalrpop::Configuration::new()
        .generate_in_source_tree()
        .process();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/syntax.lalrpop");
}

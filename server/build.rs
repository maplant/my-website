fn main() {
    println!("cargo:rerun-if-changed=../articles");
    println!("cargo:rerun-if-changed=../blog/src");
    println!("cargo:rerun-if-changed=../blog/templates");
    blog::compile("../projects.toml", "../articles", "../site").unwrap();
}

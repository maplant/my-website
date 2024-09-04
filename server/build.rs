fn main() {
    println!("cargo:rerun-if-changed=../employment.toml");
    println!("cargo:rerun-if-changed=../projects.toml");
    println!("cargo:rerun-if-changed=../articles");
    println!("cargo:rerun-if-changed=../blog/src");
    println!("cargo:rerun-if-changed=../blog/templates");
    blog::compile("../projects.toml", "../employment.toml", "../articles", "../site").unwrap();
}

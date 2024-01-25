use blog::{Styles, parse_markdown_to_html};
use askama::Template;

fn main() -> anyhow::Result<()> {
    let result = parse_markdown_to_html(
	r#"
```rust
fn test() {
  5 + 5
}
```

hello

```
wooooah
```
"#
    );

    println!("result = {}", result);
    Ok(())
}

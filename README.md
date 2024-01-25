# My Website

This repository powers my personal website, visible at [maplant.com](https://maplant.com). At it's core, it's basically a custom static site generator
bundled with a really simple Axum file server. It's a little over engineered, but it works for me. 

Pushing to the main branch of this repository automatically deploys to my digital ocean application, so anything you see here is directly
responsible for the output on the website itself.

## Directory overview:

- `articles/`: This is where my blog posts are located. They are formatted as Markdown files. Previously I was using Org mode and publishing 
  those files directly to HTML locally, but I wanted something a little simpler. The file name indicates the date in which the article is published
  and the title of the article.
- `blog/`: Static site generator. At its core, it includes a function called `compile` that builds all of the necessary HTML and CSS files.
- `server/`: Simple Axum server. Includes a `build.rs` script that calls the `blog::compile` function.
- `site/`: The output directory for all the HTML and CSS files. Files included here are static files that are not built by the `blog` program.

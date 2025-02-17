use anyhow::{anyhow, Result};
use askama::Template;
use chrono::{DateTime, NaiveDate, Utc};
use inkjet::{formatter, Highlighter, Language};
use pulldown_cmark::{CodeBlockKind, Event, Options, Tag};
use regex::Regex;
use std::fs;

#[derive(Template)]
#[template(path = "article.html")]
pub struct Article {
    date: DateTime<Utc>,
    title: String,
    filename: String,
    body: String,
    preprint: bool,
}

impl Article {
    fn from_file(filename: &str, md_contents: &str) -> Result<Option<Self>> {
        let re = Regex::new(
            r"(?<filename>(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})(?<title>(-[^-]+)+))\.md",
        )?;
        let Some(caps) = re.captures(filename) else {
            return Ok(None);
        };
        let preprint = filename.contains("preprint");
        let year: i32 = caps["year"].parse()?;
        let month: u32 = caps["month"].parse()?;
        let day: u32 = caps["day"].parse()?;
        let title = caps["title"].split("-").collect::<Vec<_>>().join(" ");
        let filename = format!("{}.html", &caps["filename"]);
        let body = parse_markdown_to_html(md_contents);
        Ok(Some(Self {
            date: NaiveDate::from_ymd_opt(year, month, day)
                .ok_or_else(|| anyhow!("Invalid year/month/day: {year}/{month}/{day}"))?
                .and_hms_opt(0, 0, 0)
                .unwrap()
                .and_utc(),
            title,
            filename,
            body,
            preprint,
        }))
    }

    fn from_dir(dir: &str) -> Result<Vec<Self>> {
        let mut output = Vec::new();
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let contents = fs::read_to_string(entry.path())?;
            let path = entry.path();
            let file_name = path.file_name().unwrap().to_str().unwrap();
            if file_name.ends_with("~") {
                continue;
            }
            let Some(article) = Self::from_file(file_name, &contents)? else {
                continue;
            };
            output.push(article);
        }
        output.sort_by(|a, b| a.date.cmp(&b.date).reverse());
        Ok(output)
    }

    fn write(&self, path: &str) -> Result<()> {
        fs::write(&format!("{path}/{}", self.filename), &self.render()?)?;
        Ok(())
    }
}

#[derive(serde::Deserialize, Clone)]
pub struct Project {
    name: String,
    description: String,
    features: Vec<String>,
    url: String,
    active: bool,
}

#[derive(Template, serde::Deserialize)]
#[template(path = "projects.html")]
pub struct Projects {
    projects: Vec<Project>,
}

impl Projects {
    fn active(&self) -> &[Project] {
        let first_inactive = self.projects.iter().position(|p| !p.active).unwrap();
        &self.projects[..first_inactive]
    }

    fn from_file(file: &str) -> Result<Self> {
        let mut projects: Self = toml::from_str(&fs::read_to_string(file)?)?;
        projects
            .projects
            .sort_by(|a, b| a.active.cmp(&b.active).reverse());
        Ok(projects)
    }

    fn write(&self, path: &str) -> Result<()> {
        fs::write(&format!("{path}/projects.html"), &self.render()?)?;
        Ok(())
    }
}

#[derive(serde::Deserialize, Clone, Debug)]
pub struct Employment {
    title: String,
    company: String,
    timeline: String,
    features: Vec<String>,
}

#[derive(serde::Deserialize, Clone, Debug)]
pub struct EmploymentHistory {
    employment: Vec<Employment>,
}

impl EmploymentHistory {
    fn from_file(file: &str) -> Result<Self> {
        Ok(toml::from_str(&fs::read_to_string(file)?)?)
    }
}

#[derive(Template)]
#[template(path = "resume.html")]
pub struct Resume {
    employment: Vec<Employment>,
    projects: Vec<Project>,
}

impl Resume {
    fn new(employment: EmploymentHistory, projects: Projects) -> Self {
        Self {
            employment: employment.employment,
            projects: projects.projects,
        }
    }

    fn write(&self, path: &str) -> Result<()> {
        fs::write(&format!("{path}/resume.html"), &self.render()?)?;
        Ok(())
    }
}

#[derive(Template)]
#[template(path = "index.html")]
pub struct Index<'a> {
    articles: &'a [Article],
    projects: &'a [Project],
}

impl<'a> Index<'a> {
    fn new(articles: &'a [Article], projects: &'a [Project]) -> Self {
        Self { articles, projects }
    }

    fn write(&self, path: &str) -> Result<()> {
        fs::write(&format!("{path}/index.html"), &self.render()?)?;
        Ok(())
    }
}

#[derive(Template)]
#[template(path = "articles.html")]
pub struct Articles<'a> {
    articles: &'a [Article],
}

impl<'a> Articles<'a> {
    fn new(articles: &'a [Article]) -> Self {
        Self { articles }
    }

    fn write(&self, path: &str) -> Result<()> {
        fs::write(&format!("{path}/articles.html"), &self.render()?)?;
        Ok(())
    }
}

#[derive(Template)]
#[template(path = "rss.xml")]
pub struct Rss<'a> {
    articles: &'a [Article],
}

impl<'a> Rss<'a> {
    fn new(articles: &'a [Article]) -> Self {
        Self { articles }
    }

    fn write(&self, path: &str) -> Result<()> {
        fs::write(&format!("{path}/rss.xml"), &self.render()?)?;
        Ok(())
    }
}

#[derive(Template)]
#[template(path = "notfound.html")]
pub struct NotFound;

impl NotFound {
    fn new() -> Self {
        Self
    }

    fn write(&self, path: &str) -> Result<()> {
        fs::write(&format!("{path}/notfound.html"), &self.render()?)?;
        Ok(())
    }
}

#[derive(Template)]
#[template(path = "styles.css", escape = "none")]
pub struct Styles {
    dark_theme: String,
    light_theme: String,
}

fn theme_to_css(theme: inkjet::theme::Theme) -> String {
    let mut css = String::new();

    css.push_str(".code {\n");
    css.push_str(&format!("color: {};\n", theme.fg.into_hex()));
    css.push_str(&format!("background-color: {};\n", theme.bg.into_hex()));
    css.push_str("}\n");

    for (name, style) in theme.styles {
        css.push_str(&format!(".{} {{\n", name));
        if let Some(color) = style.fg {
            css.push_str(&format!("color: {};\n", color.into_hex()));
        }
        if let Some(color) = style.bg {
            css.push_str(&format!("background-color: {};\n", color.into_hex()));
        }
        css.push_str("}\n");
    }

    css
}

impl Styles {
    pub fn new() -> Result<Self> {
        let dark_theme = inkjet::theme::Theme::from_helix(inkjet::theme::vendored::SOLARIZED_DARK)?;
        let light_theme =
            inkjet::theme::Theme::from_helix(inkjet::theme::vendored::SOLARIZED_LIGHT)?;

        Ok(Self {
            dark_theme: theme_to_css(dark_theme),
            light_theme: theme_to_css(light_theme),
        })
    }

    pub fn write(&self, path: &str) -> Result<()> {
        fs::write(&format!("{path}/styles.css"), &self.render()?)?;
        Ok(())
    }
}

pub fn parse_code_snippit(lang: &str, code: &str) -> Result<String> {
    let language = Language::from_token(lang)
        .ok_or_else(|| anyhow::anyhow!("No such language def: {lang}"))?;
    Ok(Highlighter::new().highlight_to_string(language, &formatter::Html, code)?)
}

pub fn parse_codeblock<'a>(
    lang: &str,
    markdown_parser: &mut impl Iterator<Item = Event<'a>>,
    output: &mut Vec<Event<'a>>,
) {
    while let Some(next) = markdown_parser.next() {
        match next {
            Event::Text(text) => {
                output.push(Event::Html(if lang.is_empty() {
                    format!(r#"<pre class="code">{text}</pre>"#).into()
                } else {
                    format!(
                        r#"<pre class="code">{}</pre>"#,
                        parse_code_snippit(lang, &text).unwrap()
                    )
                    .into()
                }));
            }
            Event::End(_) => break,
            _ => panic!(),
        }
    }
}

pub fn parse_markdown<'a>(markdown: &'a str) -> impl Iterator<Item = Event<'a>> {
    let mut parser = pulldown_cmark::Parser::new_ext(markdown, Options::all());
    let mut output = Vec::new();

    while let Some(next) = parser.next() {
        match next {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
                parse_codeblock(&lang, &mut parser, &mut output);
            }
            x => {
                output.push(x);
            }
        }
    }

    output.into_iter()
}

pub fn parse_markdown_to_html(markdown: &str) -> String {
    let mut html_output = String::new();
    pulldown_cmark::html::push_html(&mut html_output, parse_markdown(markdown));
    html_output
}

pub fn compile(
    projects_file: &str,
    employment_file: &str,
    articles_dir: &str,
    output_dir: &str,
) -> Result<()> {
    let articles = Article::from_dir(articles_dir)?;
    for article in &articles {
        article.write(output_dir)?;
    }
    let styles = Styles::new()?;
    styles.write(output_dir)?;
    let projects = Projects::from_file(projects_file)?;
    projects.write(output_dir)?;
    let index = Index::new(&articles[..articles.len().min(5)], projects.active());
    index.write(output_dir)?;
    let rss = Rss::new(&articles[..]);
    rss.write(output_dir)?;
    let articles = Articles::new(&articles[..]);
    articles.write(output_dir)?;
    let notfound = NotFound::new();
    notfound.write(output_dir)?;
    let employment = EmploymentHistory::from_file(employment_file)?;
    let resume = Resume::new(employment, projects);
    resume.write(output_dir)?;

    Ok(())
}

mod filters {
    pub fn fmt_date(date: &chrono::DateTime<chrono::Utc>) -> ::askama::Result<String> {
        Ok(format!("{}", date.format("%Y-%m-%d")))
    }

    pub fn fmt_rfc_822(date: &chrono::DateTime<chrono::Utc>) -> ::askama::Result<String> {
        Ok(date.to_rfc2822())
    }
}

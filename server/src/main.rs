use axum::{response::Redirect, routing::get, Router};
use tower_http::services::{ServeDir, ServeFile};

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route(
            "/gc.html",
            get(|| async {
                Redirect::permanent("/2020-04-25-Writing-a-Simple-Garbage-Collector-in-C.html")
            }),
        )
        .fallback_service(ServeDir::new("site").not_found_service(ServeFile::new("notfound.html")));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:8080").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

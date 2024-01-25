FROM rust:1.75-slim-buster

WORKDIR /app

COPY . .

RUN cargo build --release

EXPOSE 8080
CMD [ "./target/release/server" ]

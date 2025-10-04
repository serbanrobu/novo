# Start with image that has the Rust toolchain installed
FROM rust:1.90-alpine AS chef
USER root
# Add cargo-chef to cache dependencies
RUN apk add --no-cache musl-dev & cargo install cargo-chef
WORKDIR /app

FROM chef AS planner
COPY . .
# Capture info needed to build dependencies
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder
COPY --from=planner /app/recipe.json recipe.json
# Build dependencies - this is the caching Docker layer!
RUN cargo chef cook --release --recipe-path recipe.json
# Build application
COPY . .
RUN cargo build --release

# We do not need the Rust toolchain to run the binary!
# Start with a minimal image and copy over the binary.
FROM debian:buster-slim AS runtime
WORKDIR /app
COPY --from=builder /app/target/release/novo /usr/local/bin
ENTRYPOINT ["/bin/sh", "-c"]
CMD ["/usr/local/bin/novo serve --port=${PORT:-8000}"]

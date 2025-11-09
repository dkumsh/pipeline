# print options
default:
    @just --list --unsorted

# install cargo tools
init:
    cargo upgrade --incompatible
    cargo update

# check code
check:
    cargo check
    cargo fmt --all -- --check
    cargo clippy --all-targets --all-features

# automatically fix clippy warnings
fix:
    cargo fmt --all
    cargo clippy --allow-dirty --allow-staged --fix

# build project
build:
   cargo build --all-targets

# execute tests
test:
   cargo test

# execute benchmarks
bench:
    cargo bench

# run pipeline examples
examples:
    cargo run --example rename_example
    cargo run --example generics_contexts
    cargo run --example two_contexts
    cargo run --example early_break
    cargo run --example moving_average
    cargo run --example reset
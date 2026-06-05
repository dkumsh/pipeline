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

# run Miri (undefined-behavior checks) over the dynamic graph's unsafe store
miri:
    cargo +nightly miri test -p pipeline-graph

# Publish all crates to crates.io. Uses workspace publish (cargo >= 1.90), which
# resolves the interdependencies locally, verifies each crate, uploads them in
# dependency order, and waits for the index to propagate between dependents — so
# no manual ordering or sleeps are needed. `pipeline-example` is publish=false
# and is skipped automatically. Requires a clean git tree and `cargo login`.
#   just publish              # real publish
#   just publish --dry-run    # full dry-run: package + verify every crate, no upload
publish dry="":
    cargo publish --workspace {{dry}}

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
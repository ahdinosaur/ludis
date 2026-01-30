export LUSID_APPLY_LINUX_X86_64 := "./target/x86_64-unknown-linux-gnu/release/lusid-apply"
export LUSID_APPLY_LINUX_AARCH64 := "./target/aarch64-unknown-linux-gnu/release/lusid-apply"

build-lusid-apply:
  cargo build -p lusid-apply --target x86_64-unknown-linux-gnu --release
  # cargo build -p lusid-apply --target aarch64-unknown-linux-gnu --release

lusid-local-apply: build-lusid-apply
  cargo run -p lusid --release -- local apply --config ./examples/lusid.toml

lusid-dev-a-apply: build-lusid-apply
  cargo run -p lusid --release -- dev apply --config ./examples/lusid.toml --machine a

lusid-dev-a-ssh:
  cargo run -p lusid --release -- dev ssh --config ./examples/lusid.toml --machine a

lusid-dev-b-apply: build-lusid-apply
  cargo run -p lusid --release -- dev apply --config ./examples/lusid.toml --machine b

lusid-dev-b-ssh:
  cargo run -p lusid --release -- dev ssh --config ./examples/lusid.toml --machine b

lusid-apply-example-simple:
  cargo run -p lusid-apply -- --root . --plan ./examples/simple.lusid --params '{ "whatever": true }' --log trace

lusid-apply-example-multi:
  cargo run -p lusid-apply -- --root . --plan ./examples/multi.lusid --log trace

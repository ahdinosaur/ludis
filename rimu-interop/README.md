# rimu-interop

Bridging helpers between Rust types and [Rimu](https://rimu.dev) values.

- **`FromRimu`**: trait for parsing Rust types out of a `rimu::Value`, with a
  `from_rimu_spanned` helper that preserves source spans on both success and
  error paths.
- **`to_rimu`**: serializes any `Serialize` type into a `Spanned<Value>` carrying
  a synthetic zero-width span - used when injecting Rust-side data (like
  detected `System` info) into plan scripts.

This crate has no lusid-specific logic; it's a thin bidirectional adapter.

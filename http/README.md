# lusid-http

HTTP client for fetching remote artifacts during plan apply.

A thin wrapper around `reqwest` with:

- Gzip and Brotli decoding enabled.
- A 10-second read timeout.
- Streaming `download_file` that writes through a `.tmp` sidecar and renames on
  completion, so interrupted runs never leave a half-written file that looks
  whole.

Intentionally minimal - no retry, resume, or content verification. Callers that
need those (e.g. content-addressed fetching) should handle them at a higher
layer.

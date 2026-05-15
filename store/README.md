# lusid-store

Abstract content store for bytes referenced by a plan (e.g. source files loaded by the `file` resource).

`Store` is a multiplexer over one or more `SubStore` backends, tagged by `StoreItemId`. Today only `LocalFile` is implemented — a thin wrapper around `tokio::fs::read`.

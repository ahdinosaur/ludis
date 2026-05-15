# lusid-view

Serializable styled-text view primitives for the lusid streaming UI.

Types in lusid's domain (resource params, resources, states, changes, operations) implement [`Render`](src/render.rs) to produce a [`View`]. Views travel as JSON over `lusid-apply`'s stdout pipe. The current TUI (`lusid/src/tui.rs`) only reads their text content via `Display` - the style fields exist for a future renderer.

## View shapes

```text
View
├── Span       - styled text run (one segment)
├── Line       - Vec<Span> + optional style / alignment
├── Paragraph  - Vec<Line>  + optional style / alignment
└── Fragment   - Vec<View>  (concatenation, no separator)
```

Plus [`ViewTree`]: `Branch { view, children } | Leaf { view }` - a recursive wrapper whose `Display` delegates to [`termtree`](https://docs.rs/termtree).

[`TextStyle`] carries `{fg,bg}_color`, bold/italic/underlined/crossed_out, and underline colour. [`Color`] is the 16 standard terminal colours.

## Adding Render for your type

For types that already `Display` cleanly:

```rust
lusid_view::impl_display_render!(MyType);
```

For anything richer, implement [`Render`] by hand and return the appropriate `View` variant.

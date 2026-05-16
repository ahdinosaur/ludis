# lusid-view

Serializable text view primitives for the lusid streaming UI.

Types in lusid's domain (resource params, resources, states, changes, operations) implement [`Render`](src/render.rs) to produce a [`View`]. Views travel as JSON over `lusid-apply`'s stdout pipe.

## View shapes

```text
View
├── Span       - a text run (one segment)
├── Line       - Vec<Span>
├── Paragraph  - Vec<Line>
└── Fragment   - Vec<View>  (concatenation, no separator)
```

Plus [`ViewTree`]: `Branch { view, children } | Leaf { view }` - a recursive wrapper whose `Display` delegates to [`termtree`](https://docs.rs/termtree).

## Adding Render for your type

For types that already `Display` cleanly:

```rust
lusid_view::impl_display_render!(MyType);
```

For anything richer, implement [`Render`] by hand and return the appropriate `View` variant.

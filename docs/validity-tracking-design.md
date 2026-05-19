# Per-slot validity tracking in `Vector<V>`

Status: design / scoping. Author: pipeline-dsl maintainer.
Target version: 0.2.0.

## Goal

`Vector<V>` gains a per-slot validity bit, alongside the existing dirty bit. Stage authors get an `Option`-like read API (`get_valid`) and an explicit commit/invalidate write API; the bug class where a stage silently holds last value when its input is bad becomes structurally hard to introduce.

## Motivation

Today a stage that reads from one `Vector<I>` and writes to another `Vector<O>` looks like:

```rust
for (i, input) in inputs.iter_updated() {
    if let Some(out) = outputs.get_mut(i) {
        if let Some(v) = compute(input) {
            *out = v;
        }
        // else: out keeps its previous value silently
    }
}
```

Downstream consumers of `outputs` cannot distinguish a *fresh* good value from a *stale-from-30-cycles-ago* value held over because `compute` returned `None` repeatedly. The bug surfaces as derived signals (mids, vwaps, fair values) lying about their freshness whenever the upstream goes bad.

The fix is to make the slot itself carry validity, exposed through Option-like accessors that the type system forces the caller to unwrap.

## Non-goals

- No change to `Value<V>` semantics in v1. Defer until the `Vector<V>` pattern is settled and the cross-type story is understood.
- No removal or behaviour change of existing `Vector` methods. Pure addition; existing pipelines compile and run unchanged.
- No async or parallel changes; orthogonal.
- No change to the `#[pipeline]` / `#[stage]` macros required for the core feature. Macro authors may want a new attribute later to opt slot-writer iteration in by default, but not in v1.

## Internal data model change

Add one field to `Vector<V>`:

```rust
pub struct Vector<V> {
    data: Vec<V>,
    update_flags: BitVec,  // existing — per-slot dirty
    valid_flags: BitVec,   // NEW — per-slot validity
    indices: Vec<usize>,
    is_updated: bool,
}
```

### Semantics of `valid_flags`

- Persists across `reset()` (unlike `update_flags`). Dirty is per-cycle; validity is multi-cycle state about whether the slot has a meaningful value.
- A freshly `push`ed slot is **invalid**. Sizing the vector with `push(default)` should not pretend the slot holds a meaningful value yet. A separate `push_committed(v)` is provided for the rare case where the caller genuinely has a meaningful starting value.
- The existing "all_updated optimisation" (the trick that clears `update_flags` when all slots are dirty) applies to `update_flags` only; `valid_flags` is unaffected by it.
- `reset()` clears `update_flags` and `indices` as today, but leaves `valid_flags` untouched. A slot that was valid at end of cycle stays valid at start of next cycle (until somebody invalidates it).

## New surface area on `Vector<V>`

| Method | Effect |
|---|---|
| `commit(&mut self, i: usize, value: V)` | Write `value` to slot `i`, set valid, mark dirty. Replaces the `get_mut(i).unwrap() = v` + implicit-validity idiom. |
| `invalidate(&mut self, i: usize)` | Clear valid, mark dirty. Slot retains its prior data (invisible to readers via the validity-aware accessors). |
| `is_valid(&self, i: usize) -> bool` | Validity bit lookup. |
| `get_valid(&self, i: usize) -> Option<&V>` | Read accessor mirroring `Option::as_ref` — returns `Some` iff valid. **The primary read path under the new model.** |
| `slot_writer(&mut self, i: usize) -> SlotWriter<'_, V>` | Acquire a one-shot writer; must be `commit`-ed or `invalidate`-d. Drop without consumption panics in debug. |
| `iter_updated_valid(&self) -> impl Iterator<Item = (usize, Option<&V>)>` | Like `iter_updated`, but yields the option per slot. The "feed bad now" signal at the dirty-slot level. |
| `push_committed(&mut self, value: V)` | As `push`, but the new slot starts valid. |

### Methods that stay verbatim

`get`, `get_mut`, `get_mut_untracked`, `get_updated`, `iter`, `iter_mut`, `iter_updated`, `push`, `pop`, `clear`, `reset`, `len`, `is_empty`, `is_updated`, `all_updated`.

Code using only those keeps working; validity bits are simply not consulted. `get_mut` marks dirty as today; it does NOT touch `valid_flags`. A stage that uses `get_mut` to write a slot leaves validity at whatever it was before — so a `commit`-then-`get_mut` sequence keeps the slot valid, and a `push`-then-`get_mut` sequence keeps the slot invalid. This is documented as a foot-gun: new code should prefer `slot_writer` / `commit` / `invalidate`.

## SlotWriter

```rust
#[must_use = "SlotWriter must be consumed via commit() or invalidate()"]
pub struct SlotWriter<'a, V> {
    vector: &'a mut Vector<V>,
    index: usize,
    consumed: bool,
}

impl<'a, V> SlotWriter<'a, V> {
    pub fn commit(mut self, value: V) {
        self.vector.write_committed(self.index, value);  // sets valid + dirty
        self.consumed = true;
    }

    pub fn invalidate(mut self) {
        self.vector.mark_invalid(self.index);             // clears valid + dirty
        self.consumed = true;
    }
}

impl<'a, V> Drop for SlotWriter<'a, V> {
    fn drop(&mut self) {
        if !self.consumed {
            debug_assert!(
                false,
                "SlotWriter at index {} dropped without commit() or invalidate()",
                self.index
            );
            // Release builds: silently leak the choice. Documented; the
            // must_use lint catches it at compile time in any project
            // with -D warnings.
        }
    }
}
```

Key properties:

- `#[must_use]` — bare `vec.slot_writer(i);` is a compile-time warning, hard error under `-D warnings`.
- `commit` / `invalidate` consume `self`, so the drop check only fires when the author *truly* drops without consuming.
- Debug-build `debug_assert!` makes "forgot to choose" a loud test-time failure. Release builds don't panic — `must_use` plus tests are sufficient enforcement.

## UpdatedWritersIter

Streaming-iterator pattern (not `std::Iterator`, but works with `while let`):

```rust
pub struct UpdatedWritersIter<'a, V> { /* index source + &'a mut Vector<V> */ }

impl<'a, V> UpdatedWritersIter<'a, V> {
    pub fn next(&mut self) -> Option<(usize, Option<&V>, SlotWriter<'_, V>)> { ... }
}
```

Stage author writes:

```rust
let mut it = vwaps.iter_updated_writers();
while let Some((i, prev, writer)) = it.next() {
    // ...
}
```

A `for` loop would require either `LendingIterator` (unstable) or moving to GATs in a way that's invasive for downstream users. The `while let` form is the standard pragmatic shape today.

## Recommended stage shape

For most pipeline stages the natural pattern is **separate iteration on inputs, slot_writer on outputs**:

```rust
fn vwap(books: &Vector<Book>, vwaps: &mut Vector<f64>) {
    for (i, book_opt) in books.iter_updated_valid() {
        let writer = vwaps.slot_writer(i);
        match book_opt {
            Some(book) => writer.commit(book.vwap_f64().unwrap_or(0.0)),
            None       => writer.invalidate(),
        }
    }
}
```

`slot_writer` + `iter_updated_valid` is the everyday pattern when input and output vectors differ. A future API could add an `iter_updated_writers` helper for stages whose output dirty set is the same as the iteration set.

## Backward compatibility

- Adding `valid_flags` grows `Vector<V>` by one `BitVec` per instance. Acceptable; one bit per slot.
- `get_mut`, `iter_mut`, `push` mark dirty but leave `valid_flags` untouched per slot. Existing pipelines that use only those methods behave exactly as today; their slots' validity bits stay at whatever they were last set to (default: all-false, since slots are pushed invalid).
- `get_updated(i)` keeps its current "dirty-only" semantics. The validity-aware accessor is the new `get_valid(i)`. Two different questions, two different methods.
- The `Value<V>` type is untouched in v1.

## Test plan

Add to `pipeline/src/value/vector.rs` `tests` module:

1. **Fresh-push invalidity** — `push(42); assert!(!vec.is_valid(0)); assert_eq!(vec.get_valid(0), None);`
2. **commit roundtrip** — `vec.commit(0, 42); assert_eq!(vec.get_valid(0), Some(&42)); assert!(vec.is_updated());`
3. **invalidate roundtrip** — after commit, `vec.invalidate(0); assert_eq!(vec.get_valid(0), None); assert!(vec.is_updated());` (slot still dirty.)
4. **reset preserves validity, clears dirty** — after commit + reset, `is_valid(0)` still true, `is_updated()` is false, `get_valid(0)` returns `Some(_)`.
5. **SlotWriter consumed by commit** — no panic, slot becomes valid.
6. **SlotWriter consumed by invalidate** — no panic, slot becomes invalid, slot is dirty.
7. **SlotWriter dropped without consumption** — debug build: `#[should_panic]` test. Release build: behavioural test that the slot is unchanged and the assertion is suppressed.
8. **iter_updated_valid composition** — `push` + `invalidate` + `commit` + `reset` + `commit` pattern; verify the iterator yields what `(dirty ∧ ?)` predicts.
9. **Capacity alignment** — `Vector::with_capacity(n)` should reserve validity bits without making any out-of-bounds slot valid; subsequent `push_committed` and `pop` should keep validity aligned with data length.
10. **Cross-test with existing `get_mut`** — confirm `get_mut` after commit leaves validity intact; document the foot-gun.

`pipeline_example` should grow one new example that uses `commit` / `invalidate` end-to-end to validate the macros pass these signatures through cleanly.

## Macros

The `#[pipeline]` / `#[stage]` macros should pass through `&mut Vector<V>` parameters without change — slot-writer iteration is plain Rust inside the stage body. **One verification step**: confirm the `#[stage]` derive doesn't impose constraints that would reject the new iter shape. Existing stages keep working with `iter_updated()`, new stages opt in by calling `iter_updated_valid()` or `slot_writer()`.

Future v2 work could add a `#[stage(strict)]` attribute that requires every dirty input slot to be either committed or invalidated in the output. Out of scope for v1.

## Open questions

1. **Release-build behaviour of dropped SlotWriter**: silent (current scope) or panic-always (heavier)? Recommendation: silent in release, `debug_assert!` in debug, `#[must_use]` always. Catches it where it matters without prod-build cost.
2. **`Reset` trait interaction**: confirm the "reset clears dirty, leaves valid" rule matches the existing `Reset for Value<V>` semantics so the cross-type story is uniform once `Value<V>` joins.
3. **Version bump**: additive but adds a public type and several public methods — minor bump from 0.1.1 → 0.2.0. No semver-major needed.

## Estimated diff size

- `vector.rs`: ~+150 lines (struct field, ~7 new methods, SlotWriter, streaming iterator, doc comments)
- `vector.rs` tests: ~+150 lines (the 10 cases above)
- `pipeline_example` / new example: ~50 lines
- `README` / docs update: ~30 lines
- No `pipeline_derive` change required for v1

Total: ~400 lines of additive change, no breaking changes.

## Why this is the right shape

- **Mirrors `Option` discipline at the slot level.** The reader is forced to match on an `Option<&V>` to reach the inner value; same constraint that makes `Option` safe.
- **Validity is producer-asserted, not framework-inferred.** Auto-deriving output validity from input validity sounds clever but breaks for stages that legitimately produce valid outputs from a subset of valid inputs. Producer responsibility is uniform and obvious.
- **Validity propagation is free for downstream pipelines.** A stage two layers downstream reading via `get_valid` gets the right answer regardless of which upstream stage went bad — no per-stage staleness-input channels, no shadow vectors.
- **`Vector<V>` slot layout stays exactly `V`.** The validity bit lives in a side BitVec, not in the value itself, so `V` remains zerocopy-compatible if it was before. `Vector<Option<V>>` would not have this property.
- **β-soft enforcement (`#[must_use]` + debug-assert) catches the common bug class without paying a runtime cost in release.** Strong type-level enforcement (linear types) is left for a future iteration if it earns its keep.

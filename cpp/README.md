# pipeline (C++20)

A header-only C++ port of the dynamic front-end of the Rust `pipeline` crates:
**graph-shaped, incremental computation**. Declare typed values, register stages
that read/write them, and the framework wires a dependency DAG, runs it in
topological order, and recomputes only what changed each cycle.

This is the C++-natural slice of the Rust design — `pipeline-core` (the value
layer) + `pipeline-graph` (the runtime-assembled front-end). The static
`pipeline-dsl` front-end is intentionally **not** ported; see *What's missing*.

## Layout

| File | Rust analogue | What it is |
|---|---|---|
| `include/pipeline/value.hpp` | `pipeline-core` | `Value<T>` / `Vector<T>` — dirty/validity-tracked cells + `Resettable`/`Trackable` concepts |
| `include/pipeline/graph.hpp` | `pipeline-graph` | `Graph` / `Slot<T>` / `In`/`Out` / `Pipeline` — type-erased store, `build()` validation, `compute()` |
| `examples/telemetry_monitor.cpp` | example | runnable demo incl. demand-driven `skip_when_clean` + stats |
| `examples/validation.cpp` | — | proves the `build()`-time guards fire |

## Build & run

```sh
cmake -S . -B build && cmake --build build -j
cd build && ctest --output-on-failure
./telemetry_monitor
```

Or just: `g++ -std=c++20 -Iinclude examples/telemetry_monitor.cpp -o tele && ./tele`

## Quick taste

```cpp
Graph g;
auto temps = g.slot<Vector<double>>("temps");
auto avg   = g.slot<Value<double>>("avg");
g.external(temps);

g.stage("stats", std::make_tuple(In{temps}, Out{avg}),
    [](const Vector<double>& t, Value<double>& a) {
        double s = 0; std::size_t k = 0;
        for (std::size_t i = 0; i < t.size(); ++i)
            if (auto* v = t.get_valid(i)) { s += *v; ++k; }
        if (k) a.set(s / k); else a.invalidate();
        return Flow::Continue;
    });

Pipeline p = g.build();   // validates wiring, topo-sorts
p.at(temps).resize(3); /* feed... */ p.compute();
```

`In<T>` ports arrive as `const T&`, `Out<T>` as `T&`. The port type carries the
slot's payload type, so the store's downcast is checked by construction.

## How it maps to the Rust design

- **Type-erased store** — Rust's manual fn-pointer vtable over `NonNull<u8>`
  becomes a polymorphic `Node` base (`reset()`/`is_updated()`); `Slot<T>` carries
  `T` as a compile-time tag so `Store::get<T>` is a `static_cast` that is correct
  by construction.
- **Ports** — Rust's `macro_rules!` tuple impls become variadic templates +
  `std::apply` fold expressions.
- **`build()` validation** — intra-stage disjointness, single-writer,
  missing-producer, and deterministic topological sort (cycle detection), all as
  plain runtime algorithms, transcribed directly.
- **Demand-driven** — `stage_skip_when_clean` skips a stage in any cycle where
  none of its read inputs is dirty; a skipped stage writes nothing, so
  "unchanged" propagates to its readers. `Pipeline::stats()` reports run/skip
  counts.

## What's missing vs. Rust (deliberately)

- **No compile-time graph / compile errors.** Rust's `pipeline-dsl` reads
  function signatures via proc-macros and turns single-writer / missing-producer
  / cycles into *compile* errors. C++ has no portable equivalent today, so those
  rules are enforced at `build()` time (startup) instead — see `validation.cpp`.
- **No borrow-checker guarantees.** Stage bodies are *trusted* not to alias or
  stash the refs they're handed; the framework can't enforce aliasing-XOR-
  mutability or non-escape the way Rust does. Run under ASan/TSan and keep stage
  bodies pure by convention. The cross-stage single-writer/disjointness checks
  still hold.

## Status

Scaffold. Not ported yet: `Buckets`, parallel scatter (`par_update2`),
the HTML/DOT diagram renderer (reusable almost verbatim since it's data → markup),
and a real test suite.

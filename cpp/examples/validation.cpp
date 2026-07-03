// validation — proves the build()-time guards fire. These are the checks that
// replace Rust's compile-time enforcement: in C++ they happen at startup.
#include "pipeline/graph.hpp"

#include <cstdio>
#include <string>

using namespace pipeline;

template <class F>
static bool rejects(const char* what, F build_fn) {
    try {
        build_fn();
    } catch (const GraphError& e) {
        std::printf("  rejected %-18s -> %s\n", what, e.what());
        return true;
    }
    std::printf("  FAILED: %s was accepted\n", what);
    return false;
}

int main() {
    bool ok = true;

    ok &= rejects("single-writer", [] {
        Graph g;
        auto x = g.slot<Value<int>>("x");
        g.stage("a", std::make_tuple(Out{x}), [](Value<int>& v) { v.set(1); return Flow::Continue; });
        g.stage("b", std::make_tuple(Out{x}), [](Value<int>& v) { v.set(2); return Flow::Continue; });
        g.build();
    });

    ok &= rejects("missing-producer", [] {
        Graph g;
        auto x = g.slot<Value<int>>("x");          // never written, not external
        auto y = g.slot<Value<int>>("y");
        g.stage("a", std::make_tuple(In{x}, Out{y}),
                [](const Value<int>&, Value<int>& o) { o.set(0); return Flow::Continue; });
        g.build();
    });

    ok &= rejects("cycle", [] {
        Graph g;
        auto x = g.slot<Value<int>>("x");
        auto y = g.slot<Value<int>>("y");
        g.stage("a", std::make_tuple(In{y}, Out{x}),
                [](const Value<int>&, Value<int>& o) { o.set(0); return Flow::Continue; });
        g.stage("b", std::make_tuple(In{x}, Out{y}),
                [](const Value<int>&, Value<int>& o) { o.set(0); return Flow::Continue; });
        g.build();
    });

    ok &= rejects("intra-stage dup", [] {
        Graph g;
        auto x = g.slot<Value<int>>("x");
        g.stage("a", std::make_tuple(In{x}, Out{x}),   // reads and writes same node
                [](const Value<int>&, Value<int>& o) { o.set(0); return Flow::Continue; });
        g.build();
    });

    std::printf("%s\n", ok ? "all guards fired" : "SOME GUARDS DID NOT FIRE");
    return ok ? 0 : 1;
}

// pipeline/graph.hpp — the dynamic front-end (C++20 port of pipeline-graph).
//
// Assemble a dependency DAG at runtime: declare typed slots, register stages
// with their read/write port sets, then build() validates the wiring and
// topo-sorts it, and compute() runs stages in order recomputing only what
// changed.
//
// Type erasure: the Store keeps nodes behind a polymorphic base (reset() /
// is_updated()); a Slot<T> carries the payload type T as a compile-time tag, so
// fetch() is a checked static_cast — the same invariant Rust's `unsafe fetch`
// relies on, expressed as ordinary virtual dispatch.
//
// What this does NOT recover from the Rust design: there is no borrow checker,
// so a stage body is *trusted* not to alias or stash the refs it is handed.
// The single-writer / disjointness / missing-producer / acyclicity guarantees
// are enforced at build() time (startup), not by the compiler.
#pragma once

#include "value.hpp"

#include <cstdint>
#include <functional>
#include <memory>
#include <set>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

namespace pipeline {

enum class Access { Read, Write };

// A stage's return: keep going, or halt the rest of this cycle.
enum class Flow { Continue, Halt };

struct GraphError : std::runtime_error {
    using std::runtime_error::runtime_error;
};

// ---------------------------------------------------------------------------
// Type-erased store
// ---------------------------------------------------------------------------
struct Node {
    std::string name;
    bool        external = false;
    virtual void reset()      = 0;
    virtual bool is_updated() = 0;
    virtual ~Node()           = default;
};

template <class T>
struct TypedNode final : Node {
    T data;
    void reset() override { data.reset(); }
    bool is_updated() override { return data.is_updated(); }
};

class Store {
    std::vector<std::unique_ptr<Node>> nodes_;
    friend class Graph;
    friend class Pipeline;

public:
    // Checked downcast: the caller proves T via Slot<T>.
    template <class T>
    T& get(uint32_t id) {
        return static_cast<TypedNode<T>*>(nodes_[id].get())->data;
    }
    Node*       get_node(uint32_t id)       { return nodes_[id].get(); }
    const Node* get_node(uint32_t id) const { return nodes_[id].get(); }
    std::size_t size() const { return nodes_.size(); }
};

// Typed handle into the store. The PhantomData<T> of the Rust version is just
// the template parameter — Slot carries no T at runtime, only `id`.
template <class T>
struct Slot {
    uint32_t id = 0;
};

// Port wrappers: declare read/write intent at the call site.
template <class T> struct In  { Slot<T> slot; };   // Access::Read  -> const T&
template <class T> struct Out { Slot<T> slot; };   // Access::Write -> T&

// Port metadata + fetch (overload set, resolved per wrapper).
template <class T> std::pair<uint32_t, Access> port_meta(In<T> p)  { return {p.slot.id, Access::Read}; }
template <class T> std::pair<uint32_t, Access> port_meta(Out<T> p) { return {p.slot.id, Access::Write}; }

template <class T> const T& fetch(Store& s, In<T> p)  { return s.get<T>(p.slot.id); }
template <class T> T&       fetch(Store& s, Out<T> p) { return s.get<T>(p.slot.id); }

// ---------------------------------------------------------------------------
// Stage + Pipeline
// ---------------------------------------------------------------------------
struct StageStats {
    std::string name;
    uint64_t    ran = 0;
    uint64_t    skipped = 0;
};

struct StageDef {
    std::string                                 name;
    std::vector<std::pair<uint32_t, Access>>    ports;
    std::function<Flow(Store&)>                 run;
    bool                                        skip_when_clean = false;
    StageStats                                  stats;
};

class Pipeline {
    Store                 store_;
    std::vector<StageDef> stages_;     // in topological order
    std::vector<uint32_t> reset_ids_;  // written + external nodes
    friend class Graph;

public:
    // Access a node (e.g. to feed an external input or read an output).
    template <class T>
    T& at(Slot<T> s) { return store_.get<T>(s.id); }

    // Run all stages in topological order, then clear per-cycle dirty state on
    // written + external nodes.
    void compute() {
        for (auto& st : stages_) {
            if (st.skip_when_clean) {
                bool any = false;
                for (auto [id, acc] : st.ports)
                    if (acc == Access::Read && store_.get_node(id)->is_updated()) { any = true; break; }
                if (!any) { ++st.stats.skipped; continue; }
            }
            Flow f = st.run(store_);
            ++st.stats.ran;
            if (f == Flow::Halt) break;
        }
        for (uint32_t id : reset_ids_) store_.get_node(id)->reset();
    }

    std::vector<StageStats> stats() const {
        std::vector<StageStats> out;
        out.reserve(stages_.size());
        for (auto& st : stages_) out.push_back(st.stats);
        return out;
    }
};

// ---------------------------------------------------------------------------
// Graph builder
// ---------------------------------------------------------------------------
class Graph {
    Store                 store_;
    std::vector<StageDef> stages_;

    template <class... P, class Body>
    void add_stage(std::string name, std::tuple<P...> ports, Body body, bool skip) {
        StageDef def;
        def.name = name;
        std::apply([&](auto... p) { (def.ports.push_back(port_meta(p)), ...); }, ports);
        def.run = [ports, body](Store& s) -> Flow {
            return std::apply([&](auto... p) { return body(fetch(s, p)...); }, ports);
        };
        def.skip_when_clean = skip;
        def.stats.name = std::move(name);
        stages_.push_back(std::move(def));
    }

public:
    // Declare a typed slot, returning a handle.
    template <class T>
    Slot<T> slot(std::string name) {
        auto node = std::make_unique<TypedNode<T>>();
        node->name = std::move(name);
        uint32_t id = static_cast<uint32_t>(store_.nodes_.size());
        store_.nodes_.push_back(std::move(node));
        return Slot<T>{id};
    }

    // Mark a slot as externally fed (no producing stage; reset each cycle).
    template <class T>
    void external(Slot<T> s) { store_.nodes_[s.id]->external = true; }

    // Register a stage. `body` receives const T& for In<T> and T& for Out<T>.
    template <class... P, class Body>
    void stage(std::string name, std::tuple<P...> ports, Body body) {
        add_stage(std::move(name), ports, std::move(body), /*skip=*/false);
    }

    // As above, but skipped in any cycle where no read input is dirty.
    template <class... P, class Body>
    void stage_skip_when_clean(std::string name, std::tuple<P...> ports, Body body) {
        add_stage(std::move(name), ports, std::move(body), /*skip=*/true);
    }

    // Validate wiring + topo-sort. Throws GraphError on any violation.
    Pipeline build() {
        const int n = static_cast<int>(stages_.size());

        // (1) intra-stage disjointness + (2) single-writer.
        std::unordered_map<uint32_t, int> writer;  // node id -> stage index
        for (int i = 0; i < n; ++i) {
            std::set<uint32_t> seen;
            for (auto [id, acc] : stages_[i].ports) {
                if (!seen.insert(id).second)
                    throw GraphError("stage '" + stages_[i].name + "' references node " +
                                     std::to_string(id) + " more than once");
                if (acc == Access::Write) {
                    auto [it, ok] = writer.emplace(id, i);
                    if (!ok)
                        throw GraphError("node " + std::to_string(id) +
                                         " written by both '" + stages_[it->second].name +
                                         "' and '" + stages_[i].name + "' (single-writer)");
                }
            }
        }

        // (3) missing-producer: every read must have a writer or be external.
        for (auto& st : stages_)
            for (auto [id, acc] : st.ports)
                if (acc == Access::Read && !writer.count(id) && !store_.nodes_[id]->external)
                    throw GraphError("stage '" + st.name + "' reads node " + std::to_string(id) +
                                     " with no producer and not external");

        // (4) deterministic topological sort (writer-stage -> reader-stage).
        std::vector<std::vector<int>> succ(n);
        std::vector<int> indeg(n, 0);
        for (int r = 0; r < n; ++r)
            for (auto [id, acc] : stages_[r].ports)
                if (acc == Access::Read) {
                    auto it = writer.find(id);
                    if (it != writer.end() && it->second != r) {
                        succ[it->second].push_back(r);
                        ++indeg[r];
                    }
                }

        std::vector<int> order;
        order.reserve(n);
        std::set<int> ready;  // ordered -> deterministic by stage index
        for (int i = 0; i < n; ++i)
            if (indeg[i] == 0) ready.insert(i);
        while (!ready.empty()) {
            int u = *ready.begin();
            ready.erase(ready.begin());
            order.push_back(u);
            for (int v : succ[u])
                if (--indeg[v] == 0) ready.insert(v);
        }
        if (static_cast<int>(order.size()) != n)
            throw GraphError("stage graph contains a cycle (no valid topological order)");

        // Assemble the pipeline.
        Pipeline p;
        p.store_ = std::move(store_);
        for (int idx : order) p.stages_.push_back(std::move(stages_[idx]));
        std::set<uint32_t> resets;
        for (auto& [id, w] : writer) resets.insert(id);
        for (uint32_t id = 0; id < p.store_.size(); ++id)
            if (p.store_.get_node(id)->external) resets.insert(id);
        p.reset_ids_.assign(resets.begin(), resets.end());
        return p;
    }
};

}  // namespace pipeline

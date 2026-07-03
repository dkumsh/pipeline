// telemetry_monitor — a small runnable demo of the dynamic front-end.
//
// Graph shape (external inputs in [brackets], stages in <angle>):
//
//   [temps] ──┐
//             ├─> <stats> ──> avg ──┐
//             │                     ├─> <alarm> ──> alert
//   [limit] ──┴─────────────────────┘
//
//   stats : reads the temps Vector, writes avg (Value<double>)
//   alarm : reads avg + limit, writes alert (Value<bool>) — skip_when_clean,
//           so in a cycle where nothing it reads changed, it doesn't run.
#include "pipeline/graph.hpp"

#include <cstdio>

using namespace pipeline;

int main() {
    Graph g;

    auto temps = g.slot<Vector<double>>("temps");   // external sensor column
    auto limit = g.slot<Value<double>>("limit");    // external threshold
    auto avg   = g.slot<Value<double>>("avg");      // produced by <stats>
    auto alert = g.slot<Value<bool>>("alert");      // produced by <alarm>

    g.external(temps);
    g.external(limit);

    g.stage_skip_when_clean("stats",
            std::make_tuple(In{temps}, Out{avg}),
            [](const Vector<double>& t, Value<double>& a) {
                double sum = 0; std::size_t k = 0;
                for (std::size_t i = 0; i < t.size(); ++i)
                    if (auto* v = t.get_valid(i)) { sum += *v; ++k; }
                if (k) a.set(sum / static_cast<double>(k)); else a.invalidate();
                return Flow::Continue;
            });

    g.stage_skip_when_clean("alarm",
            std::make_tuple(In{avg}, In{limit}, Out{alert}),
            [](const Value<double>& a, const Value<double>& lim, Value<bool>& out) {
                const double* av = a.get_valid();
                const double* lv = lim.get_valid();
                if (av && lv) out.set(*av > *lv);
                return Flow::Continue;
            });

    Pipeline p = g.build();

    // Feed the threshold once; it stays valid across cycles.
    p.at(limit).set(70.0);

    auto run_cycle = [&](std::initializer_list<double> readings) {
        Vector<double>& col = p.at(temps);
        col.resize(readings.size());
        std::size_t i = 0;
        for (double r : readings) col.set(i++, r);
        p.compute();
        const bool* fired = p.at(alert).get_valid();
        const double* a = p.at(avg).get_valid();
        std::printf("avg=%5.1f  alert=%s\n",
                    a ? *a : 0.0, (fired && *fired) ? "FIRING" : "ok");
    };

    run_cycle({65, 68, 70});   // avg 67.7 -> ok
    run_cycle({80, 90, 100});  // avg 90.0 -> FIRING

    // Cycle with no new temps: stats writes nothing new, so alarm's inputs are
    // clean and it is skipped (demand-driven). alert keeps its last value.
    p.compute();

    std::puts("\n--- stage stats ---");
    for (auto& s : p.stats())
        std::printf("%-8s ran=%llu skipped=%llu\n", s.name.c_str(),
                    (unsigned long long)s.ran, (unsigned long long)s.skipped);
    return 0;
}

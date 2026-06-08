//! Async I/O harness around a **sync** pipeline.
//!
//! Real-world shape: subscribe to feeds (TCP/WS) → update the pipeline's
//! external inputs → recompute (possibly CPU-heavy) → publish the results.
//!
//! ```text
//!   [async ingest task(s)] --in--> [pipeline thread] --out--> [async publish task]
//!        decode messages            owns the Pipeline:            send to sinks
//!                                    drain+coalesce, compute()
//! ```
//!
//! The pipeline is a single-owner, serial `&mut self` state machine, so it lives
//! on its **own OS thread** — off the async runtime, so a heavy `compute()` never
//! starves tokio's workers. `flume` channels bridge the two worlds (sync
//! `recv`/`send` on the pipeline side, `*_async` on the edges). The pipeline
//! thread **drains and coalesces** a burst of updates and recomputes *once* —
//! which, with `skip_when_clean` + dirty-tracking, only runs the affected stages.
//!
//! Run: `cargo run --example async_harness`

use pipeline_dsl::{Value, pipeline};

// --- the existing SYNC pipeline: external input -> (heavy) stage -> output ----
#[pipeline(name = "Engine", external = "tick")]
mod engine {
    use super::*;
    use pipeline_dsl::stage;

    #[stage(skip_when_clean)] // recompute only when `tick` actually changed
    pub fn process(tick: &Value<f64>, #[unused] result: &mut Value<f64>) {
        let x = *tick.get_valid().expect("dirty => valid");
        result.set(expensive(x));
    }
}
fn expensive(x: f64) -> f64 {
    // Stand-in for a computationally consuming stage.
    x * 2.0
}

struct In {
    value: f64,
} // a decoded inbound message
struct Out {
    value: f64,
} // an outbound result

#[tokio::main]
async fn main() {
    // Bounded channels => backpressure. flume exposes both sync and async APIs.
    let (in_tx, in_rx) = flume::bounded::<In>(1024);
    let (out_tx, out_rx) = flume::bounded::<Out>(1024);

    // 1) PIPELINE ACTOR — dedicated thread, sync, the single owner of `Engine`.
    let pipe = std::thread::spawn(move || {
        let mut p = Engine::new();
        // Block for at least one update, then drain the rest of the burst so a
        // flurry of inputs collapses into a single recompute.
        while let Ok(first) = in_rx.recv() {
            p.tick.set(first.value);
            while let Ok(u) = in_rx.try_recv() {
                p.tick.set(u.value); // drain the burst; latest-wins for this input
            }
            p.compute().expect("compute"); // ...one recompute for the whole batch
            if let Some(&r) = p.result.get_valid() {
                let _ = out_tx.send(Out { value: r });
            }
        }
        // `in_tx` dropped => recv() errors => loop ends => `out_tx` drops.
    });

    // 2) INGEST — async task(s). Stub here; in production this is a WS/TCP
    //    subscription: `select!` over many streams, decode, reconnect, etc.
    let ingest = tokio::spawn(async move {
        for i in 1..=5 {
            in_tx
                .send_async(In { value: i as f64 })
                .await
                .expect("pipeline alive");
        }
        // `in_tx` drops here, signalling end-of-stream to the pipeline thread.
    });

    // 3) PUBLISH — async task draining results out to sinks.
    let publish = tokio::spawn(async move {
        while let Ok(out) = out_rx.recv_async().await {
            println!("publish: {}", out.value); // -> sink.send(out).await
        }
    });

    // Clean shutdown chains through the channel closes: ingest done -> in_tx
    // dropped -> pipeline thread exits -> out_tx dropped -> publish ends.
    ingest.await.unwrap();
    publish.await.unwrap();
    pipe.join().unwrap();
}

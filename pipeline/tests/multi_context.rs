use pipeline::*;

// Simple context types with counters
#[derive(Default)]
struct DbCtx {
    count: i32,
}

#[derive(Default)]
struct CacheCtx {
    total: i32,
}

/// Pipeline with two contexts (`db` and `cache`) and no pipeline variables.
/// The first stage increments the database counter; the second accumulates it into the cache.
#[pipeline(name = "TwoCtxPipeline", context = "db, cache")]
mod two_ctx {
    use super::*;

    #[stage]
    pub fn increment(db: &mut DbCtx) {
        db.count += 1;
    }

    #[stage]
    pub fn accumulate(cache: &mut CacheCtx, db: &DbCtx) {
        cache.total += db.count;
    }
}

#[test]
fn pipeline_with_two_contexts_runs() {
    let mut pipeline = TwoCtxPipeline::new();
    let mut db = DbCtx::default();
    let mut cache = CacheCtx::default();
    pipeline.compute(&mut db, &mut cache).unwrap();
    assert_eq!(db.count, 1);
    assert_eq!(cache.total, 1);
}

/// Pipeline with underscored context parameter names.  The `context` attribute lists `db, cache`,
/// but the stage signatures use `_db` and `_cache`.  `normalized_target_ident` should strip the
/// underscores when matching against the context names.
#[pipeline(name = "UnderscoreCtxPipeline", context = "db, cache")]
mod underscore_ctx {
    use super::*;
    use pipeline::stage;

    #[stage]
    pub fn increment(_db: &mut DbCtx) {
        _db.count += 1;
    }

    #[stage]
    pub fn accumulate(_cache: &mut CacheCtx, _db: &DbCtx) {
        _cache.total += _db.count;
    }
}

#[test]
fn underscore_context_parameters_work() {
    let mut pipeline = UnderscoreCtxPipeline::new();
    let mut db = DbCtx::default();
    let mut cache = CacheCtx::default();
    pipeline.compute(&mut db, &mut cache).unwrap();
    assert_eq!(db.count, 1);
    assert_eq!(cache.total, 1);
}

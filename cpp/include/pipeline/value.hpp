// pipeline/value.hpp — the value layer (C++20 port of pipeline-core).
//
// Dirty/validity-tracking cells. Two orthogonal bits of per-slot state:
//   - valid : does it currently hold a value?
//   - dirty : did it change *this cycle* (written OR invalidated)?
// reset() clears dirty; validity persists. A valid->invalid transition sets
// dirty, so "became invalid" propagates to readers just like a write.
#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <vector>

namespace pipeline {

// The two contracts the engine needs from anything it stores. Mirrors the
// Rust `Reset` / `Updated` traits.
template <class T>
concept Resettable = requires(T t) { t.reset(); };

template <class T>
concept Trackable = requires(const T t) {
    { t.is_updated() } -> std::convertible_to<bool>;
};

// ---------------------------------------------------------------------------
// Value<T> — a single dirty/validity-tracked cell (scalar analogue of one
// Vector slot). Initial state is invalid + clean.
// ---------------------------------------------------------------------------
template <class T>
class Value {
    std::optional<T> v_;
    bool dirty_ = false;

public:
    Value() = default;

    // Store a value and mark dirty this cycle.
    void set(T value) {
        v_ = std::move(value);
        dirty_ = true;
    }

    // Drop the held value. A valid->invalid transition is a change, so it marks
    // the cell dirty (lets readers see "no fresh/valid data"). No-op if empty.
    void invalidate() {
        if (v_.has_value()) {
            v_.reset();
            dirty_ = true;
        }
    }

    // Re-mark an existing value as dirty without changing it. No-op if invalid.
    void touch() {
        if (v_.has_value()) dirty_ = true;
    }

    bool is_valid() const { return v_.has_value(); }

    // Borrow iff valid, else nullptr (the C++ analogue of get_valid()).
    const T* get_valid() const { return v_ ? &*v_ : nullptr; }
    T*       get_valid()       { return v_ ? &*v_ : nullptr; }

    // Updated: changed this cycle (written or invalidated).
    bool is_updated() const { return dirty_; }

    // Reset: clear the per-cycle dirty bit; validity (the held value) persists.
    void reset() { dirty_ = false; }
};

// ---------------------------------------------------------------------------
// Vector<T> — a dense column of dirty/validity-tracked slots. Validity and dirty
// are bit-packed, one bit per slot, into 64-bit words.
// ---------------------------------------------------------------------------
template <class T>
class Vector : public std::vector<T> {
    std::vector<uint64_t> valid_;  // one bit per slot, packed
    std::vector<uint64_t> dirty_;  // one bit per slot, packed
    bool                  any_dirty_ = false;
    constexpr static std::size_t NBITS = sizeof(uint64_t) * 8;

    static std::size_t word(std::size_t i) { return i / NBITS; }
    static uint64_t    mask(std::size_t i) { return uint64_t{1} << (i % NBITS); }

public:
    Vector() = default;
    explicit Vector(std::size_t n) { resize(n); }

    void resize(std::size_t n) {
        std::size_t nw = (n + NBITS - 1) / NBITS;
        resize(n);
        valid_.assign(nw, 0);
        dirty_.assign(nw, 0);
        any_dirty_ = false;
    }

    std::size_t size() const { return data_.size(); }

    void set(std::size_t i, T value) {
        this->operator[](i) = std::move(value);
        auto w = word(i);
        auto m = mask(i);
        valid_[w] |= m;
        dirty_[w] |= m;
        any_dirty_ = true;
    }

    void invalidate(std::size_t i) {
        auto w = word(i);
        auto m = mask(i);
        if (valid_[w] & m) {
            valid_[w] &= ~m;
            dirty_[w] |= m;
            any_dirty_ = true;
        }
    }

    bool is_valid(std::size_t i) const { return (valid_[word(i)] & mask(i)) != 0; }
    bool is_dirty(std::size_t i) const { return (dirty_[word(i)] & mask(i)) != 0; }

    const T* get_valid(std::size_t i) const { return is_valid(i) ? &data_[i] : nullptr; }
    T*       get_valid(std::size_t i)       { return is_valid(i) ? &data_[i] : nullptr; }

    // Updated: any slot changed this cycle.
    bool is_updated() const { return any_dirty_; }

    // Reset: clear all per-cycle dirty bits; validity persists.
    void reset() {
        if (any_dirty_) {
            std::fill(dirty_.begin(), dirty_.end(), uint64_t{0});
            any_dirty_ = false;
        }
    }
};

static_assert(Resettable<Value<int>> && Trackable<Value<int>>);
static_assert(Resettable<Vector<int>> && Trackable<Vector<int>>);

}  // namespace pipeline

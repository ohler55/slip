package slip

import (
	"math/rand"
	"sort"
	"testing"
)

// ProvSet benchmarks: current slice+interpolation approach vs map approach.

func makeTestProvs(n int) (ProvSet, map[uint64]*Prov) {
	// Generate n prov entries with random-ish keys (list pointers).
	// Use deterministic seed for reproducibility.
	rng := rand.New(rand.NewSource(42))
	keys := make(map[uint64]bool)
	var ps ProvSet
	m := make(map[uint64]*Prov, n)
	for len(keys) < n {
		k := rng.Uint64()
		if keys[k] {
			continue
		}
		keys[k] = true
		p := &Prov{
			Filepath:    "test.lisp",
			FirstLine:   uint32(len(keys)),
			LastLine:    uint32(len(keys)),
			FirstColumn: 1,
			LastColumn:  5,
		}
		ps = append(ps, provEntry{key: k, value: p})
		m[k] = p
	}
	return ps, m
}

// Build a sorted ProvSet and a map, then benchmark lookups.

func BenchmarkProvSetLookup(b *testing.B) {
	ps, m := makeTestProvs(1000) // typical size for a medium project
	ps.Sort()

	// Collect all keys for lookup.
	keys := make([]uint64, len(ps))
	for i, e := range ps {
		keys[i] = e.key
	}

	b.Run("interpolation-search", func(b *testing.B) {
		b.ReportAllocs()
		for i := 0; i < b.N; i++ {
			k := keys[i%len(keys)]
			_ = ps.GetByKey(k)
		}
	})

	b.Run("map", func(b *testing.B) {
		b.ReportAllocs()
		for i := 0; i < b.N; i++ {
			k := keys[i%len(keys)]
			_ = m[k]
		}
	})
}

// Benchmark adds: slice append vs map insert.

func BenchmarkProvSetAdd(b *testing.B) {
	rng := rand.New(rand.NewSource(1))

	b.Run("slice-append", func(b *testing.B) {
		b.ReportAllocs()
		b.StopTimer()
		var ps ProvSet
		keys := make([]uint64, b.N)
		for i := 0; i < b.N; i++ {
			keys[i] = rng.Uint64()
		}
		b.StartTimer()
		for i := 0; i < b.N; i++ {
			ps = append(ps, provEntry{key: keys[i], value: &Prov{}})
		}
	})

	b.Run("map-insert", func(b *testing.B) {
		b.ReportAllocs()
		b.StopTimer()
		m := make(map[uint64]*Prov, b.N)
		rng2 := rand.New(rand.NewSource(1))
		keys := make([]uint64, b.N)
		for i := 0; i < b.N; i++ {
			keys[i] = rng2.Uint64()
		}
		b.StartTimer()
		for i := 0; i < b.N; i++ {
			m[keys[i]] = &Prov{}
		}
	})
}

// Benchmark full workflow: add all entries, then sort, then lookup.
// This is closer to the real usage pattern.

func BenchmarkProvSetWorkflow(b *testing.B) {
	rng := rand.New(rand.NewSource(99))
	n := 1000

	b.Run("provset-workflow", func(b *testing.B) {
		b.ReportAllocs()
		for iter := 0; iter < b.N; iter++ {
			var ps ProvSet
			keys := make([]uint64, n)
			for i := 0; i < n; i++ {
				k := rng.Uint64()
				keys[i] = k
				ps = append(ps, provEntry{key: k, value: &Prov{}})
			}
			sort.Slice(ps, func(i, j int) bool { return ps[i].key < ps[j].key })
			for i := 0; i < n; i++ {
				_ = ps.GetByKey(keys[i%len(keys)])
			}
		}
	})

	b.Run("map-workflow", func(b *testing.B) {
		b.ReportAllocs()
		for iter := 0; iter < b.N; iter++ {
			m := make(map[uint64]*Prov, n)
			keys := make([]uint64, n)
			for i := 0; i < n; i++ {
				k := rng.Uint64()
				keys[i] = k
				m[k] = &Prov{}
			}
			for i := 0; i < n; i++ {
				_ = m[keys[i%len(keys)]]
			}
		}
	})
}

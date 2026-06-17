package bench_test

import (
	"reflect"
	"testing"
	"unsafe"

	"github.com/ohler55/slip"
)

const provSetSize = 10000

func makeLists(cnt int) (map[uint64]*slip.Prov, []uint64) {
	pm := map[uint64]*slip.Prov{}
	var keys []uint64
	var lists []slip.List

	for i := 0; i < cnt; i++ {
		list := slip.List{slip.Fixnum(i)}
		lists = append(lists, list)
		key := uint64((*reflect.SliceHeader)(unsafe.Pointer(&list)).Data)
		keys = append(keys, key)
		pm[key] = &slip.Prov{
			Filepath:    "bench.lisp",
			FirstLine:   uint32(i),
			LastLine:    uint32(i),
			FirstColumn: 0,
			LastColumn:  10,
		}
	}
	return pm, keys
}

func BenchmarkProvSetLookup(b *testing.B) {
	var ps slip.ProvSet
	pm, keys := makeLists(provSetSize)
	for k, p := range pm {
		ps.AddByKey(k, p)
	}
	ps.Sort()
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		for _, key := range keys {
			_ = ps.GetByKey(key)
		}
	}
}

func BenchmarkProvMapLookup(b *testing.B) {
	pm, keys := makeLists(provSetSize)
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		for _, key := range keys {
			_ = pm[key]
		}
	}
}

func BenchmarkProvSetCreate(b *testing.B) {
	// Use the same prov to eliminate the overhead of creating a new prov each
	// time.
	prov := slip.Prov{}
	lists := make([]slip.List, provSetSize) // keep to avoid GC
	keys := make([]uint64, provSetSize)
	for n := 0; n < b.N; n++ {
		for i := 0; i < provSetSize; i++ {
			list := slip.List{slip.Fixnum(i)}
			lists[i] = list
			key := uint64((*reflect.SliceHeader)(unsafe.Pointer(&list)).Data)
			keys[i] = key
		}
	}
	b.ResetTimer()

	for n := 0; n < b.N; n++ {
		// ps := make(slip.ProvSet, 0, n*10) // about the same
		ps := make(slip.ProvSet, 0, 4096)
		for i := 0; i < provSetSize; i++ {
			ps.AddByKey(keys[i], &prov)
		}
		ps.Sort()
	}
}

func BenchmarkProvMapCreate(b *testing.B) {
	// Use the same prov to eliminate the overhead of creating a new prov each
	// time.
	prov := slip.Prov{}
	lists := make([]slip.List, provSetSize) // keep to avoid GC
	keys := make([]uint64, provSetSize)
	for n := 0; n < b.N; n++ {
		for i := 0; i < provSetSize; i++ {
			list := slip.List{slip.Fixnum(i)}
			lists[i] = list
			key := uint64((*reflect.SliceHeader)(unsafe.Pointer(&list)).Data)
			keys[i] = key
		}
	}
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		pm := map[uint64]*slip.Prov{}
		//pm := make(map[uint64]*slip.Prov, n*10) // slower
		for i := 0; i < provSetSize; i++ {
			pm[keys[i]] = &prov
		}
	}
}

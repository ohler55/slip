// Copyright (c) 2026, Peter Ohler, All rights reserved.

package slip

import (
	"reflect"
	"sort"

	"github.com/ohler55/ojg/pretty"
)

var Provenance = false

var allListProvs ProvSet

// Prov represents function provenance which include the filepath and location
// in the file of a function.
type Prov struct {
	Filepath    string
	FirstLine   uint32
	LastLine    uint32 // doubles as the index in the code reader starts slice
	FirstColumn uint16
	LastColumn  uint16
	// Use atomic.AddUint32 to change and atomic.LoadUint32 to get value.
	count uint32
}

type provEntry struct {
	key   uint64
	value *Prov
}

// ProvEntry is used by the code reader to keep track of Prov data associated
// with each list read. The ProvSet is then passed to CompileList and
// ListToFunc to attach the Prov to functions after they are created from the
// lists. A ProvSet is 6 times faster than a map[uint64]*Prov for adding to
// the set and 3 times faster for lookups.
type ProvSet []provEntry

// Add a Prov for a list without attempting to sort nor to check for
// duplicates.
func (ps ProvSet) Add(list List, value *Prov) ProvSet {
	return append(ps, provEntry{key: uint64(reflect.ValueOf(list).Pointer()), value: value})
}

// AddByKey adds a Prov without attempting to sort nor to check for
// duplicates.
func (ps ProvSet) AddByKey(key uint64, value *Prov) ProvSet {
	return append(ps, provEntry{key: key, value: value})
}

// Sort the slice. Must be called before Get().
func (ps ProvSet) Sort() {
	sort.Slice(ps, func(i, j int) bool { return ps[i].key < ps[j].key })
}

// Get a Prov at the provided list address. If none exists nil is returned.
func (ps ProvSet) Get(list List) (p *Prov) {
	return ps.GetByKey(uint64(reflect.ValueOf(list).Pointer()))
}

// GetByKey a Prov at the provided key. If none exists nil is returned.
func (ps ProvSet) GetByKey(key uint64) *Prov {
	if len(ps) == 0 {
		return nil
	}
	lo := 0
	lok := ps[lo].key
	if lok == key {
		return ps[lo].value
	}
	if key < lok {
		return nil
	}
	hi := len(ps) - 1
	hik := ps[hi].key
	if hik == key {
		return ps[hi].value
	}
	if hik < key {
		return nil
	}
	for lo < hi {
		i := lo + int((float64(hi-lo)*float64(key-lok))/float64(hik-lok))
		if i == lo {
			i++
			if hi == i {
				break
			}
		}
		k := ps[i].key
		if key < k {
			hi = i
			hik = k
			continue
		}
		if k < key {
			lo = i
			lok = k
			continue
		}
		return ps[i].value
	}
	return nil
}

// Simplify into the simple format defined in OjG.
func (ps ProvSet) Simplify() any {
	simple := make([]any, len(ps))
	for i, e := range ps {
		simple[i] = map[string]any{
			"key": e.key,
			"value": map[string]any{
				"filepath":    e.value.Filepath,
				"firstLine":   int64(e.value.FirstLine),
				"lastLine":    int64(e.value.LastLine),
				"firstColumn": int64(e.value.FirstColumn),
				"lastColumn":  int64(e.value.LastColumn),
				"count":       e.value.count,
			},
		}
	}
	return simple
}

// String returns a string representation of the instance.
func (ps ProvSet) String() string {
	return string(ps.Append(nil))
}

// Append encodes the instance into a []byte.
func (ps ProvSet) Append(b []byte) []byte {
	return append(b, pretty.SEN(ps.Simplify())...)
}

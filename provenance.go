// Copyright (c) 2026, Peter Ohler, All rights reserved.

package slip

import (
	"reflect"
	"sort"
)

// Prov repreesnts function provenance which include the filepath and location
// in the file of a function.
type Prov struct {
	Filepath    string
	FirstLine   uint32
	LastLine    uint32
	FirstColumn uint16
	LastColumn  uint16
	Count       uint32 // doubles as the index in the code reader starts slice
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

// Add add a Prov for a list without attempting to sort nor to check for
// duplicates.
func (ps ProvSet) Add(list List, value *Prov) ProvSet {
	if 0 < len(list) {
		ps = append(ps, provEntry{key: uint64(reflect.ValueOf(list).Pointer()), value: value})
	}
	return ps
}

// Sort the slice. Must be called before Get().
func (ps ProvSet) Sort() {
	sort.Slice(ps, func(i, j int) bool { return ps[i].key < ps[j].key })
}

// Get a Prov at the provided list address. If none exists nil is returned.
func (ps ProvSet) Get(list List) *Prov {
	if 0 < len(list) && 0 < len(ps) {
		key := uint64(reflect.ValueOf(list).Pointer())
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
	}
	return nil
}

// Get a Prov at the provided index. If none exists nil is returned.
func (ps ProvSet) Getx(key uint64) *Prov {
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
			"key":   e.key,
			"value": e.value,
		}
	}
	return simple
}

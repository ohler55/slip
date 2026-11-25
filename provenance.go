// Copyright (c) 2026, Peter Ohler, All rights reserved.

package slip

import (
	"sort"
	"sync"
	"sync/atomic"
	"unsafe"
)

// Provenance when true will collect provenance information on list and
// functions.
var Provenance = false

var (
	allListProvs = ProvSet{}
	provsMutex   sync.Mutex
)

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

// ProvEntry is used by the code reader to keep track of Prov data associated
// with each list read. The ProvSet is then passed to CompileList and
// ListToFunc to attach the Prov to functions after they are created from the
// lists.
type ProvSet map[unsafe.Pointer]*Prov

// ResetProvenance removes all Provs in the global provenance lookup
// table. Functions that already have a Prov are not changed but those values
// will not show up in reports.
func ResetProvenance() {
	provsMutex.Lock()
	allListProvs = ProvSet{}
	provsMutex.Unlock()
}

// ResetProvenanceUsage sets all provenance usage counts to zero.
func ResetProvenanceUsage() {
	provsMutex.Lock()
	for _, p := range allListProvs {
		p.count = 0
		atomic.StoreUint32(&p.count, 0)
	}
	provsMutex.Unlock()
}

// AddProvenance adds a Prov for a list.
func AddProvenance(list List, p *Prov) {
	provsMutex.Lock()
	allListProvs[unsafe.Pointer(unsafe.SliceData(list))] = p
	provsMutex.Unlock()
}

// Get a Prov at the provided list address. If none exists nil is returned.
func GetProvenance(list List) (p *Prov) {
	provsMutex.Lock()
	p = allListProvs[unsafe.Pointer(unsafe.SliceData(list))]
	provsMutex.Unlock()

	return
}

func DebugProvenance() any {
	simple := make([]any, 0, len(allListProvs))
	provsMutex.Lock()
	for k, p := range allListProvs {
		simple = append(simple, map[string]any{
			"key": int64(uintptr(k)),
			"value": map[string]any{
				"filepath":    p.Filepath,
				"firstLine":   int64(p.FirstLine),
				"lastLine":    int64(p.LastLine),
				"firstColumn": int64(p.FirstColumn),
				"lastColumn":  int64(p.LastColumn),
				"count":       p.count,
			},
		})
	}
	provsMutex.Unlock()
	sort.Slice(simple, func(i, j int) bool {
		return simple[i].(map[string]any)["key"].(int64) < simple[j].(map[string]any)["key"].(int64)
	})
	return simple
}

// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"runtime"
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Memstat{Function: slip.Function{Name: "memstat", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "memstat",
			Args: []*slip.DocArg{},
			Text: `__memstat__ return an association list of memory statistics.`,
		}, &Pkg)
}

// Memstat represents the memstat function.
type Memstat struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Memstat) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)
	var ms runtime.MemStats
	runtime.ReadMemStats(&ms)
	alist := make(slip.List, 0, 20)
	addToAssoc := func(key string, num uint64) {
		alist = append(alist, slip.List{slip.Symbol(key), slip.Tail{Value: slip.Fixnum(num)}})
	}
	addToAssoc("heap", ms.Alloc)
	addToAssoc("mallocs", ms.Mallocs)
	addToAssoc("frees", ms.Frees)
	addToAssoc("stack", ms.StackInuse)
	addToAssoc("heap-reserved", ms.HeapSys)
	addToAssoc("heap-inuse", ms.HeapInuse)
	addToAssoc("heap-objects", ms.HeapObjects)
	addToAssoc("gc-count", uint64(ms.NumGC))
	addToAssoc("gc-target", ms.NextGC)
	alist = append(alist,
		slip.List{slip.Symbol("last-gc"),
			slip.Tail{Value: slip.Time(time.Unix(0, int64(ms.LastGC)).UTC())}})
	addToAssoc("system", ms.Sys)
	addToAssoc("heap-idle", ms.HeapIdle)
	addToAssoc("heap-released", ms.HeapReleased)
	addToAssoc("system-stack", ms.StackSys)
	addToAssoc("cache-reserved", ms.MCacheSys)
	addToAssoc("cache-inuse", ms.MCacheInuse)
	addToAssoc("gc-reserved", ms.GCSys)
	addToAssoc("references", ms.Lookups)

	bySize := make(slip.List, 0, len(ms.BySize)+1)
	bySize = append(bySize, slip.Symbol("by-size"))
	for i := 0; i < len(ms.BySize); i++ {
		bs := ms.BySize[i]
		bySize = append(bySize, slip.List{slip.Fixnum(bs.Size), slip.Fixnum(bs.Mallocs), slip.Fixnum(bs.Frees)})
	}
	return append(alist, bySize)
}

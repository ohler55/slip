// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"
	"runtime"
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Room{Function: slip.Function{Name: "room", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "room",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "verbose",
					Type: "boolean",
					Text: "If true print more information.",
				},
			},
			Text: `__room__ prints information about memory availability and use. The optional
_verbose_ flag can be _t_ for the most detail, _nil_ for the minimum, and _:default_ for an
intermediate amount of detail.`,
		}, &slip.CLPkg)
}

// Room represents the room function.
type Room struct {
	slip.Function
}

var defaultSymbol = slip.Symbol(":default")

// Call the function with the arguments provided.
func (f *Room) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 1)
	var ms runtime.MemStats
	runtime.ReadMemStats(&ms)
	w := s.Get("*standard-output*").(io.Writer)
	// Start with the minimum.
	fmt.Fprintf(w, "Allocated heap:        %10d bytes\n", ms.Alloc)
	fmt.Fprintf(w, "Total mallocs:         %10d\n", ms.Mallocs)
	fmt.Fprintf(w, "Total frees:           %10d\n", ms.Frees)
	fmt.Fprintf(w, "Stack inuse:           %10d bytes\n", ms.StackInuse)

	var verbosity slip.Object = defaultSymbol
	if 0 < len(args) {
		verbosity = args[0]
	}
	if verbosity == nil {
		return slip.Novalue
	}
	// Add intermediate details.
	fmt.Fprintf(w, "Heap reserved:         %10d bytes\n", ms.HeapSys)
	fmt.Fprintf(w, "Heap inuse:            %10d bytes\n", ms.HeapInuse)
	fmt.Fprintf(w, "Heap:                  %10d objects\n", ms.HeapObjects)
	fmt.Fprintf(w, "GC count:              %10d\n", ms.NumGC)
	fmt.Fprintf(w, "GC target:             %10d bytes\n", ms.NextGC)
	fmt.Fprintf(w, "Last GC at:            %s\n", time.Unix(0, int64(ms.LastGC)).UTC().Format(time.RFC3339Nano))
	if verbosity == defaultSymbol {
		return slip.Novalue
	}
	// Add lots of detail.
	fmt.Fprintf(w, "System reserved:       %10d bytes\n", ms.Sys)
	fmt.Fprintf(w, "Heap idle:             %10d bytes\n", ms.HeapIdle)
	fmt.Fprintf(w, "Heap released:         %10d bytes\n", ms.HeapReleased)
	fmt.Fprintf(w, "System stack:          %10d bytes\n", ms.StackSys)
	fmt.Fprintf(w, "Cache reserved:        %10d bytes\n", ms.MCacheSys)
	fmt.Fprintf(w, "Cache inuse:           %10d bytes\n", ms.MCacheInuse)
	fmt.Fprintf(w, "GC reserved:           %10d bytes\n", ms.GCSys)
	fmt.Fprintf(w, "Pointer references:    %10d\n", ms.Lookups)
	fmt.Fprintf(w, "Allocation by size:\n")
	for i := 0; i < len(ms.BySize); i++ {
		bs := ms.BySize[i]
		// for _, bs := range ms.BySize {
		fmt.Fprintf(w, "  %10d mallocs, %10d frees for size <= %d bytes\n", bs.Mallocs, bs.Frees, bs.Size)
	}
	return slip.Novalue
}

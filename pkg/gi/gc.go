// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"runtime"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Gc{Function: slip.Function{Name: "gc", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "gc",
			Args: []*slip.DocArg{},
			Text: `__gc__ starts a GC and waits for it to finish.`,
		}, &Pkg)
}

// Gc represents the gc function.
type Gc struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Gc) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)
	runtime.GC()
	return nil
}

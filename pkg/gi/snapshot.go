// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Snapshot{Function: slip.Function{Name: "snapshot", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "snapshot",
			Args: []*slip.DocArg{
				{
					Name: "destination",
					Type: "output-stream|string|t|nil",
					Text: `The destination to write to. If _t_ then write to _*standard-output*.
If _nil_ then return a string.`,
				},
			},
			Return: "string|nil",
			Text:   `__snapshot__ TBD limitation as well as behavior`,
			Examples: []string{
				`(snapshot t)`,
			},
		}, &Pkg)
}

// Snapshot represents the snapshot function.
type Snapshot struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Snapshot) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)

	// TBD

	return nil
}

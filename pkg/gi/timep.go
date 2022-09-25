// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Timep{Function: slip.Function{Name: "timep", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "timep",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "Object to check.",
				},
			},
			Text: `__timep__ returns true if _object_ is of type __time__ and nil otherwise.`,
			Examples: []string{
				`(timep @2022-07-10T17:29:21.123456789Z) => t`,
			},
		}, &Pkg)
}

// Timep represents the timep function.
type Timep struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Timep) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(slip.Time); ok {
		return slip.True
	}
	return nil
}

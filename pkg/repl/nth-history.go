// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NthHistory{Function: slip.Function{Name: "nth-history", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nth-history",
			Args: []*slip.DocArg{
				{
					Name: "index",
					Type: "fixnum",
					Text: "Index of the history entry to return.",
				},
			},
			Return: "object",
			Text:   `__nth-history__ returns the form identified by _index_.`,
		}, &Pkg)
}

// NthHistory represents the nth-history function.
type NthHistory struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *NthHistory) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return nthStashCall(f, &TheHistory.Stash, s, args)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdar{Function: slip.Function{Name: "cdar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cdar__ returns (cdr (car arg)).`,
			Examples: []string{
				"(cdar nil) => nil",
				"(cdar '((a b))) => (d)",
				"(setq x '((a . b)))",
				"(setf (cdar x) 'z) => z",
				" x => ((a . z))",
			},
		}, &slip.CLPkg)
}

// Cdar represents the cdar function.
type Cdar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{true, false})
}

// Place a value in the first position of a list or cons.
func (f *Cdar) Place(args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{true, false}, value)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cddar{Function: slip.Function{Name: "cddar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cddar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cddar__ returns (cdr (cdr (car arg))).`,
			Examples: []string{
				"(cddar nil) => nil",
				"(cddar '((a (b c)))) => (c)",
			},
		}, &slip.CLPkg)
}

// Cddar represents the cddar function.
type Cddar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cddar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{true, false, false})
}

// Place a value in the first position of a list or cons.
func (f *Cddar) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{true, false, false}, value)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cddaar{Function: slip.Function{Name: "cddaar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cddaar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cddaar__ returns (cdr (cdr (car (car arg)))).`,
			Examples: []string{
				"(cddaar nil) => nil",
				"(cddaar '(((a b c)))) => (c)",
			},
		}, &slip.CLPkg)
}

// Cddaar represents the cddaar function.
type Cddaar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cddaar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{true, true, false, false})
}

// Place a value in the first position of a list or cons.
func (f *Cddaar) Place(args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{true, true, false, false}, value)
}

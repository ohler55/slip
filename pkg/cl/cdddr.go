// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdddr{Function: slip.Function{Name: "cdddr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdddr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the values from.",
				},
			},
			Return: "list",
			Text:   `__cdddr__ returns the (cdr (cdr (cdr arg))) of _arg_.`,
			Examples: []string{
				"(cdddr nil) => nil",
				"(cdddr '(a b c d) => (d)",
				"(cdddr '(a b)) => nil",
			},
		}, &slip.CLPkg)
}

// Cdddr represents the cdddr function.
type Cdddr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdddr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{false, false, false})
}

// Place a value in the first position of a list or cons.
func (f *Cdddr) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{false, false, false}, value)
}

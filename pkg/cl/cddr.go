// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cddr{Function: slip.Function{Name: "cddr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cddr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the values from.",
				},
			},
			Return: "list",
			Text:   `__cddr__ returns the (cdr (cdr arg)) of _arg_.`,
			Examples: []string{
				"(cddr nil) => nil",
				"(cddr '(a b c d) => (c d)",
				"(cddr '(a b)) => nil",
			},
		}, &slip.CLPkg)
}

// Cddr represents the cddr function.
type Cddr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cddr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{false, false})
}

// Place a value in the first position of a list or cons.
func (f *Cddr) Place(args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{false, false}, value)
}

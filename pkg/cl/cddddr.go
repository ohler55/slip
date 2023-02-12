// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cddddr{Function: slip.Function{Name: "cddddr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cddddr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the values from.",
				},
			},
			Return: "list",
			Text:   `__cddddr__ returns the (cdr (cdr (cdr (cdr arg)))) of _arg_.`,
			Examples: []string{
				"(cddddr nil) => nil",
				"(cddddr '(a b c d e) => (e)",
				"(cddddr '(a b)) => nil",
			},
		}, &slip.CLPkg)
}

// Cddddr represents the cddddr function.
type Cddddr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cddddr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cddGet(f, 4, args)
}

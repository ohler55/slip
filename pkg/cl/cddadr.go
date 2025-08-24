// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cddadr{Function: slip.Function{Name: "cddadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cddadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cddadr__ returns (cdr (cdr (car (cdr arg)))).`,
			Examples: []string{
				"(cddadr nil) => nil",
				"(cddadr '((a (b c d)))) => (d)",
			},
		}, &slip.CLPkg)
}

// Cddadr represents the cddadr function.
type Cddadr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cddadr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(s, f, args, []bool{false, true, false, false}, depth)
}

// Place a value in the first position of a list or cons.
func (f *Cddadr) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(s, f, args, []bool{false, true, false, false}, value)
}

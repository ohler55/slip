// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdaaar{Function: slip.Function{Name: "cdaaar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdaaar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cdaaar__ returns (cdr (car (car (car arg)))).`,
			Examples: []string{
				"(cdaaar nil) => nil",
				"(cdaaar '((((a b c))))) => (b c)",
				"(setq x '((((a . b)))))",
				"(setf (cdaaar x) 'z) => z",
				" x => ((((a z))))",
			},
		}, &slip.CLPkg)
}

// Cdaaar represents the cdaaar function.
type Cdaaar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdaaar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{true, true, true, false})
}

// Place a value in the first position of a list or cons.
func (f *Cdaaar) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{true, true, true, false}, value)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cadaar{Function: slip.Function{Name: "cadaar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cadaar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cadaar__ returns (car (cdr (car (car arg)))).`,
			Examples: []string{
				"(cadaar nil) => nil",
				"(cadaar '(((a . (b . c)) . d) . e)) => b",
				"(setq x '(((a b c))))",
				"(setf (cadaar x) 'z) => z",
				" x => (((a b z)))",
			},
		}, &slip.CLPkg)
}

// Cadaar represents the cadaar function.
type Cadaar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cadaar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{true, true, false, true})
}

// Place a value in the first position of a list or cons.
func (f *Cadaar) Place(args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{true, true, false, true}, value)
}

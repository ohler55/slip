// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caddar{Function: slip.Function{Name: "caddar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caddar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__caddar__ returns (car (cdr (cdr (car arg)))).`,
			Examples: []string{
				"(caddar nil) => nil",
				"(caddar '((a b c))) => c",
				"(setq x '((a b c)))",
				"(setf (caddar x) 'z) => z",
				" x => ((a b z))",
			},
		}, &slip.CLPkg)
}

// Caddar represents the caddar function.
type Caddar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caddar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(s, f, args, []bool{true, false, false, true}, depth)
}

// Place a value in the first position of a list or cons.
func (f *Caddar) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(s, f, args, []bool{true, false, false, true}, value)
}

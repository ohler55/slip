// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caar{Function: slip.Function{Name: "caar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The value to take the first value of the first element of.",
				},
			},
			Return: "object",
			Text:   `__caar__ returns (car (car arg)).`,
			Examples: []string{
				"(caar nil) => nil",
				"(caar '((a b c d))) => a",
				"(setq x '((a b c d)))",
				"(setf (caar x) 'z) => z",
				" x => ((z b c d))",
			},
		}, &slip.CLPkg)
}

// Caar represents the caar function.
type Caar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return cadGet(f, args, []bool{true, true})
}

// Place a value in the first position of a list or cons.
func (f *Caar) Place(args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{true, true}, value)
}

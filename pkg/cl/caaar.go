// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caaar{Function: slip.Function{Name: "caaar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caaar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The list to get a value from.",
				},
			},
			Return: "object",
			Text:   `__caaar__ returns (car (car (car arg))).`,
			Examples: []string{
				"(caaar nil) => nil",
				"(caaar '(((a b) c) d)) => a",
				"(setq x '(((a b) c) d))",
				"(setf (caaar x) 'z) => z",
				" x => (((z b) c) d)",
			},
		}, &slip.CLPkg)
}

// Caaar represents the caaar function.
type Caaar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caaar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return caaGet(f, 3, args)
}

// Place a value in the first position of a list or cons.
func (f *Caaar) Place(args slip.List, value slip.Object) {
	caaPlace(f, 3, args, value)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cadar{Function: slip.Function{Name: "cadar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cadar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cadar__ returns (car (cdr (car arg))).`,
			Examples: []string{
				"(cadar nil) => nil",
				"(cadar '((a b c))) => b",
				"(setq x '((a b c)))",
				"(setf (cadar x) 'z) => z",
				" x => ((a z c))",
			},
		}, &slip.CLPkg)
}

// Cadar represents the cadar function.
type Cadar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cadar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{true, false, true})
}

// Place a value in the first position of a list or cons.
func (f *Cadar) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{true, false, true}, value)
}

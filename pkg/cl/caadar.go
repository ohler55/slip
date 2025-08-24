// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caadar{Function: slip.Function{Name: "caadar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caadar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the second element of.",
				},
			},
			Return: "object",
			Text:   `__caadar__ returns (car (car (cdr (car arg)))).`,
			Examples: []string{
				"(caadar nil) => nil",
				"(caadar '(a b c)) => b",
				"(setq x '(a b c))",
				"(setf (caadar x) 'z) => z",
				" x => (a z c)",
			},
		}, &slip.CLPkg)
}

// Caadar represents the caadar function.
type Caadar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caadar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(s, f, args, []bool{true, false, true, true}, depth)
}

// Place a value in the first position of a list or cons.
func (f *Caadar) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(s, f, args, []bool{true, false, true, true}, value)
}

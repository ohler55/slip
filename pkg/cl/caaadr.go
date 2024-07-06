// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caaadr{Function: slip.Function{Name: "caaadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caaadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The list to get a value from.",
				},
			},
			Return: "object",
			Text:   `__caaadr__ returns (car (car (car (cdr arg)))).`,
			Examples: []string{
				"(caaadr nil) => nil",
				"(caaadr '(a ((b c) d) e)) => b",
				"(setq x '(a ((b c) d) e))",
				"(setf (caaadr x) 'z) => z",
				" x => (a ((z c) d) e)",
			},
		}, &slip.CLPkg)
}

// Caaadr represents the caaadr function.
type Caaadr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caaadr) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return cadGet(f, args, []bool{false, true, true, true})
}

// Place a value in the first position of a list or cons.
func (f *Caaadr) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{false, true, true, true}, value)
}

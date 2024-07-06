// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caadr{Function: slip.Function{Name: "caadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The list to get a value from.",
				},
			},
			Return: "object",
			Text:   `__caadr__ returns (car (car (cdr arg))).`,
			Examples: []string{
				"(caadr nil) => nil",
				"(caadr '(a (b c) d)) => b",
				"(setq x '(a (b c) d))",
				"(setf (caadr x) 'z) => z",
				" x => (a (z c) d)",
			},
		}, &slip.CLPkg)
}

// Caadr represents the caadr function.
type Caadr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caadr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{false, true, true})
}

// Place a value in the first position of a list or cons.
func (f *Caadr) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{false, true, true}, value)
}

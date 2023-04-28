// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cadddr{Function: slip.Function{Name: "cadddr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cadddr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The value to take the fourth element of.",
				},
			},
			Return: "object",
			Text:   `__cadddr__ returns (car (cdr (cdr (cdr arg)))).`,
			Examples: []string{
				"(cadddr nil) => nil",
				"(cadddr '(a b c d)) => d",
				"(setq x '(a b c d))",
				"(setf (cadddr x) 'z) => z",
				" x => (a b d z)",
			},
		}, &slip.CLPkg)
}

// Cadddr represents the cadddr function.
type Cadddr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cadddr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{false, false, false, true})
}

// Place a value in the first position of a list or cons.
func (f *Cadddr) Place(args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{false, false, false, true}, value)
}

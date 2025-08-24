// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caddr{Function: slip.Function{Name: "caddr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caddr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The value to take the third element of.",
				},
			},
			Return: "object",
			Text:   `__caddr__ returns (car (cdr (cdr arg))).`,
			Examples: []string{
				"(caddr nil) => nil",
				"(caddr '(a b c)) => c",
				"(setq x '(a b c))",
				"(setf (caddr x) 'd) => d",
				" x => (a b d)",
			},
		}, &slip.CLPkg)
}

// Caddr represents the caddr function.
type Caddr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caddr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(s, f, args, []bool{false, false, true}, depth)
}

// Place a value in the first position of a list or cons.
func (f *Caddr) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(s, f, args, []bool{false, false, true}, value)
}

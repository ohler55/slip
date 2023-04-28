// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caaddr{Function: slip.Function{Name: "caaddr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caaddr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the second element of.",
				},
			},
			Return: "object",
			Text:   `__caaddr__ returns (car (car (cdr (cdr arg)))).`,
			Examples: []string{
				"(caaddr nil) => nil",
				"(caaddr '(a b (c d))) => c",
				"(setq x '(a b (c d)))",
				"(setf (caaddr x) 'z) => z",
				" x => (a b (z d))",
			},
		}, &slip.CLPkg)
}

// Caaddr represents the caaddr function.
type Caaddr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caaddr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{false, false, true, true})
}

// Place a value in the first position of a list or cons.
func (f *Caaddr) Place(args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{false, false, true, true}, value)
}

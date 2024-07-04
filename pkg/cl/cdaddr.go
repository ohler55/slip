// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdaddr{Function: slip.Function{Name: "cdaddr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdaddr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cdaddr__ returns (cdr (car (cdr (cdr arg)))).`,
			Examples: []string{
				"(cdaddr nil) => nil",
				"(cdaddr '(a b (c d))) => (d)",
				"(setq x '(a b (c . d)))",
				"(setf (cdaddr x) 'z) => z",
				" x => (a b (c . z))",
			},
		}, &slip.CLPkg)
}

// Cdaddr represents the cdaddr function.
type Cdaddr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdaddr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{false, false, true, false})
}

// Place a value in the first position of a list or cons.
func (f *Cdaddr) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{false, false, true, false}, value)
}

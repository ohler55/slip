// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdaar{Function: slip.Function{Name: "cdaar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdaar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cdaar__ returns (cdr (car (car arg))).`,
			Examples: []string{
				"(cdaar nil) => nil",
				"(cdaar '(((a b c)))) => (b c)",
				"(setq x '(((a . b))))",
				"(setf (cdaar x) 'z) => z",
				" x => (((a z)))",
			},
		}, &slip.CLPkg)
}

// Cdaar represents the cdaar function.
type Cdaar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdaar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{true, true, false})
}

// Place a value in the first position of a list or cons.
func (f *Cdaar) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{true, true, false}, value)
}

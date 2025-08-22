// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdadar{Function: slip.Function{Name: "cdadar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdadar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cdadar__ returns (car (cdr (car (cdr arg)))).`,
			Examples: []string{
				"(cdadar nil) => nil",
				"(cdadar '((a (b c)))) => (c)",
				"(setq x '((a (b . c))))",
				"(setf (cdadar x) 'z) => z",
				" x => ((a (b . z)))",
			},
		}, &slip.CLPkg)
}

// Cdadar represents the cdadar function.
type Cdadar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdadar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(s, f, args, []bool{true, false, true, false}, depth)
}

// Place a value in the first position of a list or cons.
func (f *Cdadar) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(s, f, args, []bool{true, false, true, false}, value)
}

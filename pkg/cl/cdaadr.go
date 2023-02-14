// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdaadr{Function: slip.Function{Name: "cdaadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdaadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cdaadr__ returns (cdr (car (car (cdr arg)))).`,
			Examples: []string{
				"(cdaadr nil) => nil",
				"(cdaadr '(a ((b . c)))) => c",
				"(setq x '(a ((b . c))))",
				"(setf (cdaadr x) 'z) => z",
				" x => (a ((b z)))",
			},
		}, &slip.CLPkg)
}

// Cdaadr represents the cdaadr function.
type Cdaadr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdaadr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{false, true, true, false})
}

// Place a value in the first position of a list or cons.
func (f *Cdaadr) Place(args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{false, true, true, false}, value)
}

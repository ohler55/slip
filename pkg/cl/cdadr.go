// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdadr{Function: slip.Function{Name: "cdadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cdadr__ returns (cdr (car (cdr arg))).`,
			Examples: []string{
				"(cdadr nil) => nil",
				"(cdadr '(a (b c))) => (d)",
				"(setq x '(a (b . c)))",
				"(setf (cdadr x) 'z) => z",
				" x => (a (b . z))",
			},
		}, &slip.CLPkg)
}

// Cdadr represents the cdadr function.
type Cdadr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdadr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(f, args, []bool{false, true, false})
}

// Place a value in the first position of a list or cons.
func (f *Cdadr) Place(args slip.List, value slip.Object) {
	cadPlace(f, args, []bool{false, true, false}, value)
}

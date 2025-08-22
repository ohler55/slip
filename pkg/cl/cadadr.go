// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cadadr{Function: slip.Function{Name: "cadadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cadadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The object to take the value from.",
				},
			},
			Return: "object",
			Text:   `__cadadr__ returns (car (cdr (car (cdr arg)))).`,
			Examples: []string{
				"(cadadr nil) => nil",
				"(cadadr '(a (b c d))) => c",
				"(setq x '(a (b c d)))",
				"(setf (cadadr x) 'z) => z",
				" x => (a (b z d))",
			},
		}, &slip.CLPkg)
}

// Cadadr represents the cadadr function.
type Cadadr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cadadr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(s, f, args, []bool{false, true, false, true}, depth)
}

// Place a value in the first position of a list or cons.
func (f *Cadadr) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(s, f, args, []bool{false, true, false, true}, value)
}

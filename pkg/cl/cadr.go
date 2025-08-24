// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cadr{Function: slip.Function{Name: "cadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the second element of.",
				},
			},
			Return: "object",
			Text:   `__cadr__ returns (car (cdr arg)).`,
			Examples: []string{
				"(cadr nil) => nil",
				"(cadr '(a b c)) => b",
				"(setq x '(a b c))",
				"(setf (cadr x) 'z) => z",
				" x => (a z c)",
			},
		}, &slip.CLPkg)
}

// Cadr represents the cadr function.
type Cadr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cadr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cadGet(s, f, args, []bool{false, true}, depth)
}

// Place a value in the first position of a list or cons.
func (f *Cadr) Place(s *slip.Scope, args slip.List, value slip.Object) {
	cadPlace(s, f, args, []bool{false, true}, value)
}

// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Funcall{Function: slip.Function{Name: "funcall", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "funcall",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call.",
				},
				{Name: "&rest"},
				{
					Name: "args",
					Type: "object",
					Text: "The arguments to the _function_.",
				},
			},
			Return: "nil",
			Text:   `__funcall__ calls _function_ with the _args_.`,
			Examples: []string{
				`(funcall (lambda (x y) (+ x y)) 1 2) => 3"`,
				`(funcall '+ 1 2 3) => 6"`,
			},
		}, &slip.CLPkg)
}

// Funcall represents the funcall function.
type Funcall struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Funcall) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, -1)
	fn := args[0]
	d2 := depth + 1
	caller := ResolveToCaller(s, fn, d2)

	return caller.Call(s, args[1:], d2)
}

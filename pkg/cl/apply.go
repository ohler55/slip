// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Apply{Function: slip.Function{Name: "apply", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "apply",
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
					Text: "The arguments to the _function_. The last argument must be a _list_.",
				},
			},
			Return: "nil",
			Text:   `__apply__ calls _function_ with the _args_.`,
			Examples: []string{
				`(apply (lambda (x y) (+ x y)) '(1 2)) => 3"`,
				`(apply '+ 1 2 '(3)) => 6"`,
			},
		}, &slip.CLPkg)
}

// Apply represents the apply function.
type Apply struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Apply) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	fn := args[0]
	d2 := depth + 1
	caller := resolveToCaller(s, fn, d2)
	var larg slip.List
	switch ta := args[len(args)-1].(type) {
	case nil:
		// ok, empty list
	case slip.List:
		larg = ta
	default:
		slip.PanicType("last argument", ta, "list")
	}
	cargs := make(slip.List, len(args)-2+len(larg))
	copy(cargs, args[1:])
	copy(cargs[:len(args)-1], larg)

	return caller.Call(s, cargs, d2)
}

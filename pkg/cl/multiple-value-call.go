// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MultipleValueCall{
				Function: slip.Function{Name: "multiple-value-call", Args: args, SkipEval: []bool{false, true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "multiple-value-call",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "function-designator",
					Text: "A function to apply to the remaining arguments.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate for arguments to _function_.",
				},
			},
			Return: "object",
			Text: `__multiple-value-call__ calls the _function_ with the results of _forms_.
I a form evalues to multiple-values then it is expanded before being used as arguments
to _function_.`,
			Examples: []string{
				"(multiple-value-call #'list (floor 5 3) (floor 13 4)) => (1 2 3 1)",
			},
		}, &slip.CLPkg)
}

// MultipleValueCall represents the multiple-value-call function.
type MultipleValueCall struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MultipleValueCall) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, -1)
	d2 := depth + 1
	caller := ResolveToCaller(s, args[0], d2)
	var xargs slip.List
	for i, arg := range args[1:] {
		if list, ok := arg.(slip.List); ok {
			arg = slip.ListToFunc(s, list, d2)
			f.Args[i+1] = arg
		}
		v := s.Eval(arg, d2)
		if vs, ok := v.(slip.Values); ok {
			xargs = append(xargs, vs...)
		} else {
			xargs = append(xargs, v)
		}
	}
	return caller.Call(s, xargs, d2)
}

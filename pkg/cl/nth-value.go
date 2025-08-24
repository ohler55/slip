// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NthValue{Function: slip.Function{Name: "nth-value", Args: args, SkipEval: []bool{false, true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nth-value",
			Args: []*slip.DocArg{
				{
					Name: "n",
					Type: "integer",
					Text: "The index into the _value_.",
				},
				{
					Name: "value",
					Type: "value",
					Text: "The value to return the nth element of.",
				},
			},
			Return: "object",
			Text:   `__nth-value__ returns _n_ element of _value_ or _nil_ if outside the bounds of the _value_.`,
			Examples: []string{
				"(nth-value 0 nil) => nil",
				"(nth-value 1 (values 'a 'b 'c)) => b",
				" x => (a d c)",
			},
		}, &slip.CLPkg)
}

// NthValue represents the nth-value function.
type NthValue struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *NthValue) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	arg1 := slip.EvalArg(s, args, 1, depth+1)

	if values, ok := arg1.(slip.Values); ok {
		var num slip.Integer
		if num, ok = args[0].(slip.Integer); !ok {
			slip.TypePanic(s, depth, "n", args[0], "integer")
		}
		n := int(num.Int64())
		if 0 <= n && n < len(values) {
			result = values[n]
		}
	}
	return
}

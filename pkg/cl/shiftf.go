// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Shiftf{Function: slip.Function{Name: "shiftf", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "shiftf",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "places",
					Type: "place",
					Text: "Places to get values from and to set values to.",
				},
			},
			Return: "nil",
			Text: `__shiftf__ modifies the values in _places_ by evaluating each to produce
a list of values 1 to n. Values 1 through n-1 are then placed in place2 to placen. Value n
is placed in plave1.`,
			Examples: []string{
				"(let ((x '(1 2 3 4 5)))",
				" (shiftf (nth 1 x) (nth 2 x) (nth 3 x))",
				" x) => (1 4 2 3 5)",
			},
		}, &slip.CLPkg)
}

// Shiftf represents the shiftf function.
type Shiftf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Shiftf) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	values := make(slip.List, len(args))
	for i := range args {
		values[i] = slip.EvalArg(s, args, i, depth)
		if _, ok := args[i].(slip.Placer); !ok {
			slip.TypePanic(s, depth, "places", args[i], "place")
		}
	}
	for i := len(args) - 1; 0 < i; i-- {
		callPlace(s, args[i].(slip.Placer), values[i-1], depth)
	}
	callPlace(s, args[0].(slip.Placer), values[len(values)-1], depth)

	return nil
}

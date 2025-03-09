// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Rotatef{Function: slip.Function{Name: "rotatef", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "rotatef",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "places",
					Type: "place",
					Text: "Places to get values from and to set values to.",
				},
			},
			Return: "nil",
			Text: `__rotatef__ modifies the values in _places_ by evaluating each to produce
a list of values 1 to n. Values 2 through n are then placed in place1 to place(n-1). Value
1 is placed in plaven.`,
			Examples: []string{
				"(let ((x '(1 2 3 4 5)))",
				" (rotatef (nth 1 x) (nth 2 x) (nth 3 x))",
				" x) => (1 3 4 2 5)",
			},
		}, &slip.CLPkg)
}

// Rotatef represents the rotatef function.
type Rotatef struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Rotatef) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	values := make(slip.List, len(args))
	for i := range args {
		values[i] = slip.EvalArg(s, args, i, depth)
		if _, ok := args[i].(slip.Placer); !ok {
			slip.PanicType("places", args[i], "place")
		}
	}
	for i := len(args) - 2; 0 <= i; i-- {
		callPlace(s, args[i].(slip.Placer), values[i+1], depth)
	}
	callPlace(s, args[len(args)-1].(slip.Placer), values[0], depth)

	return nil
}

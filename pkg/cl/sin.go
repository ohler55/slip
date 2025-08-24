// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Sin{Function: slip.Function{Name: "sin", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sin",
			Args: []*slip.DocArg{
				{
					Name: "radians",
					Type: "number",
					Text: "The number of radians to take the sine of.",
				},
			},
			Return: "nil",
			Text:   `__sin__ returns the sine of the _radians_.`,
			Examples: []string{
				"(sin (/ pi 2)) => 1.0",
			},
		}, &slip.CLPkg)
}

// Sin represents the sin function.
type Sin struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Sin) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Sin(real.RealValue()))
	} else {
		slip.TypePanic(s, depth, "radians", args[0], "number")
	}
	return
}

// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Asinh{Function: slip.Function{Name: "asinh", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "asinh",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number return the inverse hyperbolic sine of.",
				},
			},
			Return: "number",
			Text:   `__asinh__ returns the inverse hyperbolic sine of the _number_.`,
			Examples: []string{
				"(asinh 1.5) => 1.1947632172871094",
			},
		}, &slip.CLPkg)
}

// Asinh represents the asinh function.
type Asinh struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Asinh) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Asinh(real.RealValue()))
	} else {
		slip.TypePanic(s, depth, "number", args[0], "number")
	}
	return
}

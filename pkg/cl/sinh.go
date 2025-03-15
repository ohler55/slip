// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Sinh{Function: slip.Function{Name: "sinh", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sinh",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number return the hyperbolic sine of.",
				},
			},
			Return: "number",
			Text:   `__sinh__ returns the hyperbolic sine of the _number_.`,
			Examples: []string{
				"(sinh 0) => 0",
			},
		}, &slip.CLPkg)
}

// Sinh represents the sinh function.
type Sinh struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Sinh) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Sinh(real.RealValue()))
	} else {
		slip.PanicType("number", args[0], "number")
	}
	return
}

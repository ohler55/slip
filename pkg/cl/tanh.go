// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Tanh{Function: slip.Function{Name: "tanh", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "tanh",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number return the hyperbolic tangent of.",
				},
			},
			Return: "number",
			Text:   `__tanh__ returns the hyperbolic tangent of the _number_.`,
			Examples: []string{
				"(tanh 0) => 0",
			},
		}, &slip.CLPkg)
}

// Tanh represents the tanh function.
type Tanh struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Tanh) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Tanh(real.RealValue()))
	} else {
		slip.PanicType("number", args[0], "number")
	}
	return
}

// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Tan{Function: slip.Function{Name: "tan", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "tan",
			Args: []*slip.DocArg{
				{
					Name: "radians",
					Type: "number",
					Text: "The number of radians to take the tangent of.",
				},
			},
			Return: "nil",
			Text:   `__tan__ returns the tangent of the _radians_.`,
			Examples: []string{
				"(tan 0.5) => 0.5463025",
			},
		}, &slip.CLPkg)
}

// Tan represents the tan function.
type Tan struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Tan) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Tan(real.RealValue()))
	} else {
		slip.PanicType("radians", args[0], "number")
	}
	return
}

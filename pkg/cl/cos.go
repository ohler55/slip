// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cos{Function: slip.Function{Name: "cos", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cos",
			Args: []*slip.DocArg{
				{
					Name: "radians",
					Type: "number",
					Text: "The number of radians to take the cosine of.",
				},
			},
			Return: "nil",
			Text:   `__cos__ returns the cosine of the _radians_.`,
			Examples: []string{
				"(cos pi) => -1.0",
			},
		}, &slip.CLPkg)
}

// Cos represents the cos function.
type Cos struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Cos) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Cos(real.RealValue()))
	} else {
		slip.PanicType("radians", args[0], "number")
	}
	return
}

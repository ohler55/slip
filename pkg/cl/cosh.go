// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cosh{Function: slip.Function{Name: "cosh", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cosh",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number return the hyperbolic cosine of.",
				},
			},
			Return: "number",
			Text:   `__cosh__ returns the hyperbolic cosine of the _number_.`,
			Examples: []string{
				"(cosh 0) => 1",
			},
		}, &slip.CLPkg)
}

// Cosh represents the cosh function.
type Cosh struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cosh) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Cosh(real.RealValue()))
	} else {
		slip.PanicType("number", args[0], "number")
	}
	return
}

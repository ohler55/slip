// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Atanh{Function: slip.Function{Name: "atanh", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "atanh",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number return the inverse hyperbolic sine of.",
				},
			},
			Return: "number",
			Text:   `__atanh__ returns the inverse hyperbolic sine of the _number_.`,
			Examples: []string{
				"(atanh 0.5) => 0.5493061443340548",
			},
		}, &slip.CLPkg)
}

// Atanh represents the atanh function.
type Atanh struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Atanh) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Atanh(real.RealValue()))
	} else {
		slip.TypePanic(s, depth, "number", args[0], "number")
	}
	return
}

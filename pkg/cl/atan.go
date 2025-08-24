// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Atan{Function: slip.Function{Name: "atan", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "atan",
			Args: []*slip.DocArg{
				{
					Name: "number1",
					Type: "number",
					Text: "The number to take the arc tangent of.",
				},
				{Name: "&optional"},
				{
					Name: "number2",
					Type: "number",
				},
			},
			Return: "number",
			Text:   `__atan__ returns the arc tangent of the _number1_.`,
			Examples: []string{
				"(atan 1.0) => 0.7853982",
			},
		}, &slip.CLPkg)
}

// Atan represents the atan function.
type Atan struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Atan) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	if n1, ok := args[0].(slip.Real); ok {
		if 1 < len(args) {
			if n2, ok2 := args[1].(slip.Real); ok2 {
				result = slip.DoubleFloat(math.Atan2(n1.RealValue(), n2.RealValue()))
			} else {
				slip.TypePanic(s, depth, "number2", args[1], "number")
			}
		} else {
			result = slip.DoubleFloat(math.Atan(n1.RealValue()))
		}
	} else {
		slip.TypePanic(s, depth, "number1", args[0], "number")
	}
	return
}

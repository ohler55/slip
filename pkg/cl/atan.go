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
					Name: "number",
					Type: "number",
					Text: "The number to take the arc tangent of.",
				},
			},
			Return: "nil",
			Text:   `__atan__ returns the arc tangent of the _number_.`,
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
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Atan(real.RealValue()))
	} else {
		slip.PanicType("number", args[0], "number")
	}
	return
}

// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Acos{Function: slip.Function{Name: "acos", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "acos",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to take the arc cosine of.",
				},
			},
			Return: "nil",
			Text:   `__acos__ returns the arc cosine of the _number_.`,
			Examples: []string{
				"(acos 0.5) => 1.0471975511965976",
			},
		}, &slip.CLPkg)
}

// Acos represents the acos function.
type Acos struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Acos) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Acos(real.RealValue()))
	} else {
		slip.PanicType("number", args[0], "number")
	}
	return
}

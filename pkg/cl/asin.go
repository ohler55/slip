// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Asin{Function: slip.Function{Name: "asin", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "asin",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to take the arc sine of.",
				},
			},
			Return: "nil",
			Text:   `__asin__ returns the arc sine of the _number_.`,
			Examples: []string{
				"(asin 1.0) => 1.5707964",
			},
		}, &slip.CLPkg)
}

// Asin represents the asin function.
type Asin struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Asin) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Asin(real.RealValue()))
	} else {
		slip.PanicType("number", args[0], "number")
	}
	return
}

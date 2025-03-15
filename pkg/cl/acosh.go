// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Acosh{Function: slip.Function{Name: "acosh", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "acosh",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number return the inverse hyperbolic cosine of.",
				},
			},
			Return: "number",
			Text:   `__acosh__ returns the inverse hyperbolic cosine of the _number_.`,
			Examples: []string{
				"(acosh 1.1) => 0.4435682543851154",
			},
		}, &slip.CLPkg)
}

// Acosh represents the acosh function.
type Acosh struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Acosh) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if real, ok := args[0].(slip.Real); ok {
		result = slip.DoubleFloat(math.Acosh(real.RealValue()))
	} else {
		slip.PanicType("number", args[0], "number")
	}
	return
}

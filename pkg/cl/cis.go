// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cis{Function: slip.Function{Name: "cis", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cis",
			Args: []*slip.DocArg{
				{
					Name: "radians",
					Type: "real",
					Text: "The radians to determine cis of.",
				},
			},
			Return: "complex",
			Text: `__cis__ returns _e^i*_ radians which is a __complex__ where the real part
is the cosine of _radians_ and the imaginary part is the sine of _radians.`,
			Examples: []string{
				"(cis 0) => #C(1 0)",
			},
		}, &slip.CLPkg)
}

// Cis represents the cis function.
type Cis struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cis) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if real, ok := args[0].(slip.Real); ok {
		sin, cos := math.Sincos(real.RealValue())
		result = slip.Complex(complex(cos, sin))
	} else {
		slip.PanicType("number", args[0], "number")
	}
	return
}

// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Phase{Function: slip.Function{Name: "phase", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "phase",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to determine the phase of.",
				},
			},
			Return: "integer",
			Text: `__phase__ returns then phase of _numer_ which is defined as
 (phase x) = (atan (imagpart x) (realpart x))
`,
			Examples: []string{
				"(phase 1) => 0",
				"(phase 0) => 0",
				"(phase #C(0 1)) => 1.5707964",
			},
		}, &slip.CLPkg)
}

// Phase represents the phase function.
type Phase struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Phase) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	var (
		i float64
		r float64
	)
	switch ta := args[0].(type) {
	case slip.Complex:
		i = imag(ta)
		r = real(ta)
	case slip.Real:
		r = ta.RealValue()
	default:
		slip.PanicType("rational", args[0], "rational")
	}
	return slip.DoubleFloat(math.Atan2(i, r))
}

// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ScaleFloat{Function: slip.Function{Name: "scale-float", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "scale-float",
			Args: []*slip.DocArg{
				{
					Name: "float",
					Type: "float",
					Text: "A float to scale.",
				},
				{
					Name: "integer",
					Type: "fixnum",
					Text: "The amount to scale.",
				},
			},
			Return: "float",
			Text: `__scale-float__ returns the _float_ scaled by a base (2) to the _integer_ exponent
following the formula:
 (* _float_ (expt 2.0 _integer_))
`,
			Examples: []string{
				"(scale-float 1.5 2) => 6.0",
			},
		}, &slip.CLPkg)
}

// ScaleFloat represents the scale-float function.
type ScaleFloat struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ScaleFloat) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	var flt float64
	if r, ok := args[0].(slip.Float); ok {
		flt = r.RealValue()
	} else {
		slip.TypePanic(s, depth, "float", args[0], "real")
	}
	i, ok := args[1].(slip.Integer)
	if !ok {
		slip.TypePanic(s, depth, "integer", args[1], "integer")
	}
	return slip.DoubleFloat(flt * math.Pow(2.0, float64(i.Int64())))
}

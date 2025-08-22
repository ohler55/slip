// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Float{Function: slip.Function{Name: "float", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "float",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to convert to a _float_.",
				},
				{Name: slip.AmpOptional},
				{
					Name: "prototype",
					Type: "float",
					Text: "The _float_ type to convert to.",
				},
			},
			Return: "nil",
			Text: `__float__ returns the _number_ converted to a _float_.
Unlike CL the float type if no prototype is provided is a _double-float_`,
			Examples: []string{
				"(float 5) => 5.0",
				"(float 5/4 0.0s0) => 1.25s0",
			},
		}, &slip.CLPkg)
}

// Float represents the float function.
type Float struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Float) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	var f64 float64
	switch ta := args[0].(type) {
	case *slip.LongFloat:
		if 1 < len(args) {
			if _, ok := args[0].(*slip.LongFloat); ok {
				return ta
			}
		}
		f64 = ta.RealValue()
	case slip.Real:
		f64 = ta.RealValue()
	default:
		slip.TypePanic(s, depth, "number", ta, "number")
	}
	if 1 < len(args) {
		switch ta := args[1].(type) {
		case slip.SingleFloat:
			return slip.SingleFloat(f64)
		case slip.DoubleFloat:
			// continue as if no prototype
		case *slip.LongFloat:
			return (*slip.LongFloat)(big.NewFloat(f64))
		default:
			slip.TypePanic(s, depth, "prototype", ta, "float")
		}
	}
	return slip.DoubleFloat(f64)
}

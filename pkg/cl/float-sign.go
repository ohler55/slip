// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FloatSign{Function: slip.Function{Name: "float-sign", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "float-sign",
			Args: []*slip.DocArg{
				{
					Name: "float-1",
					Type: "float",
					Text: "The float to return the sign of.",
				},
				{Name: "&optional"},
				{
					Name: "float-2",
					Type: "float",
				},
			},
			Return: "fixnum",
			Text: `__float-sign__ returns a number z such that _z_ and _float-1_ have
the same sign and also such that _z_ and _float-2_ have the same absolute value. If
_float-2_ is not supplied, its value is (float 1.0 of the same type as _float-1_).`,
			Examples: []string{
				"(float-sign 3.0) => 1.0",
				"(float-sign -3.0) => -1.0",
				"(float-sign -3.0 2.0) => -2.0",
			},
		}, &slip.CLPkg)
}

// FloatSign represents the float-sign function.
type FloatSign struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FloatSign) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	f2 := 1.0
	if 1 < len(args) {
		if fa, ok := args[1].(slip.Float); ok {
			f2 *= fa.RealValue()
			if f2 < 0.0 {
				f2 = -f2
			}
		} else {
			slip.TypePanic(s, depth, "float-2", args[1], "float")
		}
	}
	switch ta := args[0].(type) {
	case slip.DoubleFloat:
		if 0.0 <= ta {
			result = slip.DoubleFloat(f2)
		} else {
			result = slip.DoubleFloat(-f2)
		}
	case slip.SingleFloat:
		if 0.0 <= ta {
			result = slip.SingleFloat(f2)
		} else {
			result = slip.SingleFloat(-f2)
		}
	case *slip.LongFloat:
		if 0 <= (*big.Float)(ta).Sign() {
			result = (*slip.LongFloat)(big.NewFloat(f2))
		} else {
			result = (*slip.LongFloat)(big.NewFloat(-f2))
		}
	default:
		slip.TypePanic(s, depth, "float-1", args[0], "float")
	}
	return
}

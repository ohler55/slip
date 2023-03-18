// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"math/cmplx"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Expt{Function: slip.Function{Name: "expt", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "expt",
			Args: []*slip.DocArg{
				{
					Name: "base",
					Type: "number",
					Text: "The number to use as the base for raising to the power of _power_.",
				},
				{
					Name: "power",
					Type: "number",
					Text: "The power to raise _base_ to.",
				},
			},
			Return: "nil",
			Text:   `__expt__ returns _base_ raised to _power_.`,
			Examples: []string{
				"(expt 2 3) => 8",
				"(expt 4 1/2) => 2",
			},
		}, &slip.CLPkg)
}

// Expt represents the expt function.
type Expt struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Expt) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	if base, ok := args[0].(slip.Fixnum); ok {
		if pow, ok2 := args[1].(slip.Fixnum); ok2 {
			x := math.Pow(float64(base), float64(pow))
			if (-1.0 < x && x < 1.0) || float64(math.MaxInt64) < x || x < float64(math.MinInt64) {
				return slip.DoubleFloat(x)
			}
			return slip.Fixnum(x)
		}
	}
	switch base := args[0].(type) {
	case slip.Real:
		switch pow := args[1].(type) {
		case slip.Real:
			result = slip.DoubleFloat(math.Pow(base.RealValue(), pow.RealValue()))
		case slip.Complex:
			result = slip.Complex(cmplx.Pow(complex(base.RealValue(), 0.0), complex128(pow)))
		default:
			slip.PanicType("power", pow, "number")
		}
	case slip.Complex:
		switch pow := args[1].(type) {
		case slip.Real:
			result = slip.Complex(cmplx.Pow(complex128(base), complex(pow.RealValue(), 0.0)))
		case slip.Complex:
			result = slip.Complex(cmplx.Pow(complex128(base), complex128(pow)))
		default:
			slip.PanicType("power", pow, "number")
		}
	default:
		slip.PanicType("base", base, "number")
	}
	return
}

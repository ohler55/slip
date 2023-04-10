// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"math/big"
	"math/cmplx"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Exp{Function: slip.Function{Name: "exp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "exp",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to raise _e_ to the power of.",
				},
			},
			Return: "nil",
			Text:   `__exp__ returns _e_ raised to the power of _number_.`,
			Examples: []string{
				"(exp 1) => 2.7182817",
				"(exp 2) => 7.389056",
			},
		}, &slip.CLPkg)
}

// Exp represents the exp function.
type Exp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Exp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case slip.Fixnum:
		result = slip.DoubleFloat(math.Exp(float64(ta)))
	case slip.SingleFloat:
		result = slip.DoubleFloat(math.Exp(float64(ta)))
	case slip.DoubleFloat:
		result = slip.DoubleFloat(math.Exp(float64(ta)))
	case *slip.LongFloat:
		f64, _ := (*big.Float)(ta).Float64()
		result = slip.DoubleFloat(math.Exp(f64))
	case *slip.Bignum:
		var z big.Float
		_ = z.SetInt((*big.Int)(ta))
		f64, _ := z.Float64()
		result = slip.DoubleFloat(math.Exp(f64))
	case *slip.Ratio:
		f64, _ := (*big.Rat)(ta).Float64()
		result = slip.DoubleFloat(math.Exp(f64))
	case slip.Complex:
		result = slip.Complex(cmplx.Exp(complex128(ta)))
	default:
		slip.PanicType("number", ta, "number")
	}
	return
}

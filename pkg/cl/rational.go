// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Rational{Function: slip.Function{Name: "rational", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "rational",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: "The number to convert to a _rational_.",
				},
			},
			Return: "boolean",
			Text:   `__rational__ returns _numbers_ converted to a rational.`,
			Examples: []string{
				"(rational 1/2) => 1/2",
				"(rational 0.1) => 1/10",
			},
		}, &slip.CLPkg)
}

// Rational represents the rational function.
type Rational struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Rational) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Fixnum, *slip.Bignum, *slip.Ratio:
		result = ta
	case slip.SingleFloat:
		var z big.Rat
		result = (*slip.Ratio)(z.SetFloat64(float64(ta)))
	case slip.DoubleFloat:
		var z big.Rat
		_ = z.SetFloat64(float64(ta))
		result = ratReduce(&z)
	case *slip.LongFloat:
		var z big.Rat
		rat, _ := (*big.Float)(ta).Rat(&z)
		result = ratReduce(rat)
	default:
		slip.PanicType("number", ta, "real")
	}
	return
}

func ratReduce(rat *big.Rat) (result slip.Object) {
	if rat.IsInt() {
		zi := rat.Num()
		if zi.IsInt64() {
			result = slip.Fixnum(zi.Int64())
		} else {
			result = (*slip.Bignum)(zi)
		}
	} else {
		result = (*slip.Ratio)(rat)
	}
	return
}

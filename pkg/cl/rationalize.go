// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Rationalize{Function: slip.Function{Name: "rationalize", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "rationalize",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: "The number to convert to a _rationalize_.",
				},
			},
			Return: "raio|integer",
			Text:   `__rationalize__ returns _numbers_ converted to a rational.`,
			Examples: []string{
				"(rationalize 1/2) => 1/2",
				"(rationalize 0.1) => 1/10",
			},
		}, &slip.CLPkg)
}

// Rationalize represents the rationalize function.
type Rationalize struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Rationalize) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Fixnum, *slip.Bignum, *slip.Ratio:
		result = ta
	case slip.SingleFloat:
		result = floatRat(float64(ta))
	case slip.DoubleFloat:
		result = floatRat(float64(ta))
	case *slip.LongFloat:
		var z big.Rat
		rat, _ := (*big.Float)(ta).Rat(&z)
		result = ratReduce(rat)
	default:
		slip.TypePanic(s, depth, "number", ta, "real")
	}
	return
}

const (
	maxDenom = 1000000000000000000
	maxNum   = math.MaxInt64 / 10
	ratAcc   = 0.1e-16
)

func floatRat(flt float64) (result slip.Object) {
	var neg bool

	den := int64(1)
	if flt < 0.0 {
		neg = true
		flt = -flt
	}
	for ; flt < 1.0 && den <= maxDenom; den *= 10 {
		flt *= 10.0
	}
	if 1.0 <= flt && flt < maxNum {
		for ; den <= maxDenom; den *= 10 {
			dif := flt - float64(int64(flt))
			if dif < (flt * ratAcc) {
				if neg {
					flt = -flt
				}
				return (*slip.Ratio)(big.NewRat(int64(flt), den))
			}
			flt *= 10.0
		}
	}
	var z big.Rat
	if neg {
		flt = -flt
	}
	_ = z.SetFloat64(flt)

	return ratReduce(&z)
}

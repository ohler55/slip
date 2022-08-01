// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Add{Function: slip.Function{Name: "+", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "+",
			Args: []*slip.DocArg{
				{
					Name: "numbers",
					Type: "number",
					Text: "The number to take the sum of.",
				},
			},
			Return: "number",
			Text:   `__+__ returns the um of the _numbers_.`,
			Examples: []string{
				"(1+ 5) => 5",
				"(1+ 2/3 + 3) => 11/3",
			},
		}, &slip.CLPkg)
}

// Add represents the add function.
type Add struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Add) Call(s *slip.Scope, args slip.List, depth int) (sum slip.Object) {
	sum = slip.Fixnum(0)
	var arg slip.Object
	for pos := len(args) - 1; 0 <= pos; pos-- {
		arg, sum = normalizeNumber(args[pos], sum)
		switch ta := arg.(type) {
		case slip.Fixnum:
			sum = ta + sum.(slip.Fixnum)
		case slip.SingleFloat:
			sum = ta + sum.(slip.SingleFloat)
		case slip.DoubleFloat:
			sum = ta + sum.(slip.DoubleFloat)
		case *slip.LongFloat:
			sum = (*slip.LongFloat)(((*big.Float)(sum.(*slip.LongFloat))).Add(
				(*big.Float)(sum.(*slip.LongFloat)),
				(*big.Float)(ta)),
			)
		case *slip.Bignum:
			sum = (*slip.Bignum)(((*big.Int)(sum.(*slip.Bignum))).Add((*big.Int)(sum.(*slip.Bignum)), (*big.Int)(ta)))
		case *slip.Ratio:
			sum = (*slip.Ratio)(((*big.Rat)(sum.(*slip.Ratio))).Add((*big.Rat)(sum.(*slip.Ratio)), (*big.Rat)(ta)))
		case slip.Complex:
			sum = slip.Complex(complex128(sum.(slip.Complex)) + complex128(ta))
		}
	}
	return
}

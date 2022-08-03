// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Multiply{Function: slip.Function{Name: "*", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "*",
			Args: []*slip.DocArg{
				{
					Name: "numbers",
					Type: "number",
					Text: "The number to take the product of.",
				},
			},
			Return: "number",
			Text:   `__+__ returns the product of the _numbers_.`,
			Examples: []string{
				"(* 5) => 5",
				"(* 2/3 3) => 2",
			},
		}, &slip.CLPkg)
}

// Multiply represents the multiply function.
type Multiply struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Multiply) Call(s *slip.Scope, args slip.List, depth int) (product slip.Object) {
	product = slip.Fixnum(1)
	var arg slip.Object
	for pos := len(args) - 1; 0 <= pos; pos-- {
		arg, product = normalizeNumber(args[pos], product)
		switch ta := arg.(type) {
		case slip.Fixnum:
			product = ta * product.(slip.Fixnum)
		case slip.SingleFloat:
			product = ta * product.(slip.SingleFloat)
		case slip.DoubleFloat:
			product = ta * product.(slip.DoubleFloat)
		case *slip.LongFloat:
			syncFloatPrec(ta, product.(*slip.LongFloat))
			product = (*slip.LongFloat)(((*big.Float)(product.(*slip.LongFloat))).Mul(
				(*big.Float)(product.(*slip.LongFloat)),
				(*big.Float)(ta)),
			)
		case *slip.Bignum:
			product = (*slip.Bignum)(((*big.Int)(product.(*slip.Bignum))).Mul((*big.Int)(product.(*slip.Bignum)), (*big.Int)(ta)))
		case *slip.Ratio:
			product = (*slip.Ratio)(((*big.Rat)(product.(*slip.Ratio))).Mul((*big.Rat)(product.(*slip.Ratio)), (*big.Rat)(ta)))
		case slip.Complex:
			product = slip.Complex(complex128(product.(slip.Complex)) * complex128(ta))
		}
	}
	return
}

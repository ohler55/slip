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
				{Name: "&rest"},
				{
					Name: "numbers",
					Type: "number",
					Text: "The number to take the sum of.",
				},
			},
			Return: "number",
			Text:   `__+__ returns the sum of the _numbers_.`,
			Examples: []string{
				"(+ 5) => 5",
				"(+ 2/3 3) => 11/3",
			},
		}, &slip.CLPkg)
}

// Add represents the add function.
type Add struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Add) Call(s *slip.Scope, args slip.List, depth int) (sum slip.Object) {
	sum = slip.Fixnum(0)
	for _, arg := range args {
		sum = addNumbers(arg, sum)
	}
	return
}

func addNumbers(n0, n1 slip.Object) slip.Object {
	n0, n1 = normalizeNumber(n0, n1)
	switch t0 := n0.(type) {
	case slip.Fixnum:
		n1 = t0 + n1.(slip.Fixnum)
	case slip.SingleFloat:
		n1 = t0 + n1.(slip.SingleFloat)
	case slip.DoubleFloat:
		n1 = t0 + n1.(slip.DoubleFloat)
	case *slip.LongFloat:
		syncFloatPrec(t0, n1.(*slip.LongFloat))
		n1 = (*slip.LongFloat)(((*big.Float)(n1.(*slip.LongFloat))).Add(
			(*big.Float)(n1.(*slip.LongFloat)),
			(*big.Float)(t0)),
		)
	case *slip.Bignum:
		n1 = (*slip.Bignum)(((*big.Int)(n1.(*slip.Bignum))).Add((*big.Int)(n1.(*slip.Bignum)), (*big.Int)(t0)))
	case *slip.Ratio:
		n1 = (*slip.Ratio)(((*big.Rat)(n1.(*slip.Ratio))).Add((*big.Rat)(n1.(*slip.Ratio)), (*big.Rat)(t0)))
	case slip.Complex:
		n1 = slip.Complex(complex128(n1.(slip.Complex)) + complex128(t0))
	}
	return n1
}

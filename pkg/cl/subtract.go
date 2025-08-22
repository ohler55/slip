// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Subtract{Function: slip.Function{Name: "-", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "-",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "numbers",
					Type: "number",
					Text: "The number to take the difference of.",
				},
			},
			Return: "number",
			Text:   `__-__ returns the difference of the _numbers_.`,
			Examples: []string{
				"(- 5) => -5",
				"(- 3 2/3) => 7/3",
			},
		}, &slip.CLPkg)
}

// Subtract represents the subtract function.
type Subtract struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Subtract) Call(s *slip.Scope, args slip.List, depth int) (dif slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	var arg slip.Object
	for pos, a := range args {
		if dif == nil {
			dif = a
			if _, ok := dif.(slip.Number); !ok {
				slip.TypePanic(s, depth, "numbers", dif, "number")
			}
			if pos == len(args)-1 {
				switch td := dif.(type) {
				case slip.Fixnum:
					dif = -td
				case slip.SingleFloat:
					dif = -td
				case slip.DoubleFloat:
					dif = -td
				case *slip.LongFloat:
					dif = (*slip.LongFloat)((*big.Float)(td).Neg((*big.Float)(td)))
				case *slip.Bignum:
					dif = (*slip.Bignum)((*big.Int)(td).Neg((*big.Int)(td)))
				case *slip.Ratio:
					dif = (*slip.Ratio)((*big.Rat)(td).Neg((*big.Rat)(td)))
				case slip.Complex:
					dif = slip.Complex(-complex128(td))
				}
				return
			}
			continue
		}
		arg, dif = slip.NormalizeNumber(a, dif)
		switch ta := arg.(type) {
		case slip.Fixnum:
			dif = dif.(slip.Fixnum) - ta
		case slip.SingleFloat:
			dif = dif.(slip.SingleFloat) - ta
		case slip.DoubleFloat:
			dif = dif.(slip.DoubleFloat) - ta
		case *slip.LongFloat:
			syncFloatPrec(ta, dif.(*slip.LongFloat))
			dif = (*slip.LongFloat)(((*big.Float)(dif.(*slip.LongFloat))).Sub(
				(*big.Float)(dif.(*slip.LongFloat)),
				(*big.Float)(ta)),
			)
		case *slip.Bignum:
			dif = (*slip.Bignum)(((*big.Int)(dif.(*slip.Bignum))).Sub((*big.Int)(dif.(*slip.Bignum)), (*big.Int)(ta)))
		case *slip.Ratio:
			dif = (*slip.Ratio)(((*big.Rat)(dif.(*slip.Ratio))).Sub((*big.Rat)(dif.(*slip.Ratio)), (*big.Rat)(ta)))
		case slip.Complex:
			dif = slip.Complex(complex128(dif.(slip.Complex)) - complex128(ta))
		}
	}
	return
}

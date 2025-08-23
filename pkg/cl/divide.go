// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Divide{Function: slip.Function{Name: "/", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "/",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "numbers",
					Type: "number",
					Text: "The number to take the quotient of.",
				},
			},
			Return: "number",
			Text:   `__/__ returns the quotient of the _numbers_.`,
			Examples: []string{
				"(/ 5) => 1/5",
				"(/ 3 2/3) => 9/2",
			},
		}, &slip.CLPkg)
}

// Divide represents the divide function.
type Divide struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Divide) Call(s *slip.Scope, args slip.List, depth int) (quot slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	for pos, a := range args {
		if quot == nil {
			quot = a
			if _, ok := quot.(slip.Number); !ok {
				slip.TypePanic(s, depth, "numbers", quot, "number")
			}
			if pos == len(args)-1 {
				switch td := quot.(type) {
				case slip.Fixnum:
					switch td {
					case 1:
						quot = td
					case 0:
						slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
					default:
						quot = (*slip.Ratio)(big.NewRat(1, int64(td)))
					}
				case slip.SingleFloat:
					if td == 0.0 {
						slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
					}
					quot = 1.0 / td
				case slip.DoubleFloat:
					if td == 0.0 {
						slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
					}
					quot = 1.0 / td
				case *slip.LongFloat:
					if (*big.Float)(td).Sign() == 0 {
						slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
					}
					one := big.NewFloat(1.0)
					one.SetPrec((*big.Float)(td).Prec())
					quot = (*slip.LongFloat)(one.Quo(one, (*big.Float)(td)))
				case *slip.Bignum:
					bi := (*big.Int)(td)
					if bi.Sign() == 0 {
						slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
					}
					if bi.IsInt64() && bi.Int64() == 1 {
						quot = td
					} else {
						var z big.Rat
						quot = (*slip.Ratio)(z.SetFrac(big.NewInt(1), (*big.Int)(td)))
					}
				case *slip.Ratio:
					if (*big.Rat)(td).Sign() == 0 {
						slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
					}
					quot = (*slip.Ratio)((*big.Rat)(td).Inv((*big.Rat)(td)))
				case slip.Complex:
					quot = slip.Complex(complex(1, 0) / complex128(td))
				}
				return
			}
			continue
		}
		var arg slip.Object
		arg, quot = slip.NormalizeNumber(a, quot)
		switch ta := arg.(type) {
		case slip.Fixnum:
			if ta == 0 {
				slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
			}
			if quot.(slip.Fixnum)%ta == 0 {
				quot = quot.(slip.Fixnum) / ta
			} else {
				quot = (*slip.Ratio)(big.NewRat(int64(quot.(slip.Fixnum)), int64(ta)))
			}
		case slip.SingleFloat:
			if ta == 0.0 {
				slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
			}
			quot = quot.(slip.SingleFloat) / ta
		case slip.DoubleFloat:
			if ta == 0.0 {
				slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
			}
			quot = quot.(slip.DoubleFloat) / ta
		case *slip.LongFloat:
			if (*big.Float)(ta).Sign() == 0 {
				slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
			}
			syncFloatPrec(ta, quot.(*slip.LongFloat))
			quot = (*slip.LongFloat)(((*big.Float)(quot.(*slip.LongFloat))).Quo(
				(*big.Float)(quot.(*slip.LongFloat)),
				(*big.Float)(ta)),
			)
		case *slip.Bignum:
			if (*big.Int)(ta).Sign() == 0 {
				slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
			}
			var z big.Int
			var zz big.Int
			q, r := zz.QuoRem((*big.Int)(quot.(*slip.Bignum)), (*big.Int)(ta), &z)
			if r.Sign() == 0 {
				quot = (*slip.Bignum)(q)
			} else {
				var zr big.Rat
				quot = (*slip.Ratio)(zr.SetFrac((*big.Int)(quot.(*slip.Bignum)), (*big.Int)(ta)))
			}
		case *slip.Ratio:
			if (*big.Rat)(ta).Sign() == 0 {
				slip.DivisionByZeroPanic(s, depth, slip.Symbol("/"), args, "divide by zero")
			}
			quot = (*slip.Ratio)(((*big.Rat)(quot.(*slip.Ratio))).Quo((*big.Rat)(quot.(*slip.Ratio)), (*big.Rat)(ta)))
		case slip.Complex:
			quot = slip.Complex(complex128(quot.(slip.Complex)) / complex128(ta))
		}
	}
	return
}

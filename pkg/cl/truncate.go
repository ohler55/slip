// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Truncate{Function: slip.Function{Name: "truncate", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "truncate",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: `The number to take the quotient of and then rounded
to the next integer towards zero.`,
				},
			},
			Return: "integer,real",
			Text: `__truncate__ returns the quotient of the _numbers_ rounded
toward zero as well as the remainder.`,
			Examples: []string{
				"(truncate 5.4) => 5, 0.4",
				"(truncate 3/2) => 1, 1/2",
				"(truncate 5 2) => 2, 11",
			},
		}, &slip.CLPkg)
}

// Truncate represents the truncate function.
type Truncate struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Truncate) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return truncate(f, args)
}

func truncate(f slip.Object, args slip.List) slip.Values {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	num := args[len(args)-1]
	if _, ok := num.(slip.Number); !ok {
		slip.PanicType("number", num, "real")
	}
	var (
		div slip.Object = slip.Fixnum(1)
		q   slip.Object
		r   slip.Object
	)
	if 1 < len(args) {
		div = args[0]
	}
	num, div = normalizeNumber(num, div)
	switch tn := num.(type) {
	case slip.Fixnum:
		q = tn / div.(slip.Fixnum)
		r = tn - q.(slip.Fixnum)*div.(slip.Fixnum)
	case slip.SingleFloat:
		q = tn / div.(slip.SingleFloat)
		q = slip.Fixnum(math.Trunc(float64(q.(slip.SingleFloat))))
		r = tn - slip.SingleFloat(q.(slip.Fixnum))*div.(slip.SingleFloat)
	case slip.DoubleFloat:
		q = tn / div.(slip.DoubleFloat)
		q = slip.Fixnum(math.Trunc(float64(q.(slip.DoubleFloat))))
		r = tn - slip.DoubleFloat(q.(slip.Fixnum))*div.(slip.DoubleFloat)
	case *slip.LongFloat:
		syncFloatPrec(tn, div.(*slip.LongFloat))
		var (
			zq big.Float
			zp big.Float
			zr big.Float
		)
		_ = zq.Quo((*big.Float)(tn), (*big.Float)(div.(*slip.LongFloat)))
		bi, _ := zq.Int(nil)
		q = (*slip.Bignum)(bi)
		_ = zq.SetInt(bi)
		_ = zp.Mul(&zq, (*big.Float)(div.(*slip.LongFloat)))
		r = (*slip.LongFloat)(zr.Sub((*big.Float)(tn), &zp))
	case *slip.Bignum:
		var (
			zr big.Int
			zq big.Int
		)
		_, _ = zq.QuoRem((*big.Int)(tn), (*big.Int)(div.(*slip.Bignum)), &zr)
		q = (*slip.Bignum)(&zq)
		r = (*slip.Bignum)(&zr)

	case *slip.Ratio:
		// TBD
		var (
			zr big.Rat
			zq big.Rat
			bi big.Int
			zb big.Rat
			zp big.Rat
		)
		_ = zq.Quo((*big.Rat)(tn), (*big.Rat)(div.(*slip.Ratio)))
		_ = bi.Quo(zq.Num(), zq.Denom())
		_ = zb.SetInt(&bi)
		_ = zp.Mul(&zb, (*big.Rat)(div.(*slip.Ratio)))
		_ = zr.Sub((*big.Rat)(tn), &zp)
		q = (*slip.Bignum)(&bi)
		r = (*slip.Ratio)(&zr)
	case slip.Complex:
		slip.PanicType("number", tn, "real")
	}
	return slip.Values{r, q}
}

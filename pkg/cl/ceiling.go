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
			f := Ceiling{Function: slip.Function{Name: "ceiling", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ceiling",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: `The number to take the quotient of and then rounded
to the next integer towards positive infinity.`,
				},
				{Name: "&optional"},
				{
					Name: "divisor",
					Type: "real",
					Text: `The number to divide the _number_ by. The default is 1.`,
				},
			},
			Return: "integer,real",
			Text: `__ceiling__ returns the quotient of the _numbers_ rounded
toward positive infinity as well as the remainder.`,
			Examples: []string{
				"(ceiling 5.4) => 6, -0.6",
				"(ceiling 3/2) => 2, -1/2",
				"(ceiling 5 2) => 3, -1",
			},
		}, &slip.CLPkg)
}

// Ceiling represents the ceiling function.
type Ceiling struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Ceiling) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return ceiling(f, args)
}

func ceiling(f slip.Object, args slip.List) slip.Values {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	num := args[0]
	if _, ok := num.(slip.Number); !ok {
		slip.PanicType("number", num, "real")
	}
	var (
		div slip.Object = slip.Fixnum(1)
		q   slip.Object
		r   slip.Object
	)
	if 1 < len(args) {
		div = args[1]
	}
	num, div = slip.NormalizeNumber(num, div)

	switch tn := num.(type) {
	case slip.Fixnum:
		q = tn / div.(slip.Fixnum)
		r = tn - q.(slip.Fixnum)*div.(slip.Fixnum)
		if 0 < div.(slip.Fixnum) {
			if 0 < r.(slip.Fixnum) {
				q = q.(slip.Fixnum) + slip.Fixnum(1)
				r = r.(slip.Fixnum) - div.(slip.Fixnum)
			}
		} else if r.(slip.Fixnum) < 0 {
			q = q.(slip.Fixnum) + slip.Fixnum(1)
			r = r.(slip.Fixnum) - div.(slip.Fixnum)
		}
	case slip.SingleFloat:
		q = tn / div.(slip.SingleFloat)
		q = slip.Fixnum(math.Ceil(float64(q.(slip.SingleFloat))))
		r = tn - slip.SingleFloat(q.(slip.Fixnum))*div.(slip.SingleFloat)
	case slip.DoubleFloat:
		q = tn / div.(slip.DoubleFloat)
		q = slip.Fixnum(math.Ceil(float64(q.(slip.DoubleFloat))))
		r = tn - slip.DoubleFloat(q.(slip.Fixnum))*div.(slip.DoubleFloat)
	case *slip.LongFloat:
		syncFloatPrec(tn, div.(*slip.LongFloat))
		var quo big.Float
		_ = quo.Quo((*big.Float)(tn), (*big.Float)(div.(*slip.LongFloat)))
		bi, acc := quo.Int(nil)
		switch acc {
		case big.Exact:
			q = (*slip.Bignum)(bi)
			r = (*slip.LongFloat)(big.NewFloat(0.0))
		case big.Below:
			q = (*slip.Bignum)(bi.Add(bi, big.NewInt(1)))
			var (
				zq big.Float
				zp big.Float
				zr big.Float
			)
			_ = zq.SetInt(bi)
			_ = zp.Mul(&zq, (*big.Float)(div.(*slip.LongFloat)))
			r = (*slip.LongFloat)(zr.Sub((*big.Float)(tn), &zp))
		case big.Above:
			q = (*slip.Bignum)(bi)
			var (
				zp big.Float
				zr big.Float
			)
			_ = quo.SetInt(bi)
			_ = zp.Mul(&quo, (*big.Float)(div.(*slip.LongFloat)))
			r = (*slip.LongFloat)(zr.Sub((*big.Float)(tn), &zp))
		}
	case *slip.Bignum:
		var (
			zr big.Int
			zq big.Int
		)
		_, _ = zq.QuoRem((*big.Int)(tn), (*big.Int)(div.(*slip.Bignum)), &zr)
		d := (*big.Int)(div.(*slip.Bignum))
		switch zr.Sign() {
		case 0:
			q = (*slip.Bignum)(&zq)
			r = (*slip.Bignum)(&zr)
		case -1:
			if d.Sign() == 1 {
				q = (*slip.Bignum)(&zq)
				r = (*slip.Bignum)(&zr)
			} else {
				_ = zq.Add(&zq, big.NewInt(1))
				q = (*slip.Bignum)(&zq)
				var zp big.Int
				_ = zp.Mul(&zq, d)
				r = (*slip.Bignum)(zr.Sub((*big.Int)(tn), &zp))
			}
		case 1:
			if d.Sign() == 1 {
				q = (*slip.Bignum)(zq.Add(&zq, big.NewInt(1)))
				var zp big.Int
				_ = zp.Mul(&zq, d)
				r = (*slip.Bignum)(zr.Sub((*big.Int)(tn), &zp))
			} else {
				q = (*slip.Bignum)(&zq)
				r = (*slip.Bignum)(&zr)
			}
		}
	case *slip.Ratio:
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
		d := (*big.Rat)(div.(*slip.Ratio))
		switch zr.Sign() {
		case 0:
			q = (*slip.Bignum)(&bi)
			r = slip.Fixnum(0)
		case -1:
			if d.Sign() == 1 {
				q = (*slip.Bignum)(&bi)
				r = (*slip.Ratio)(&zr)
			} else {
				q = (*slip.Bignum)(bi.Add(&bi, big.NewInt(1)))
				_ = zb.SetInt(&bi)
				_ = zp.Mul(&zb, (*big.Rat)(div.(*slip.Ratio)))
				r = (*slip.Ratio)(zr.Sub((*big.Rat)(tn), &zp))
			}
		case 1:
			if d.Sign() == 1 {
				q = (*slip.Bignum)(bi.Add(&bi, big.NewInt(1)))
				_ = zb.SetInt(&bi)
				_ = zp.Mul(&zb, (*big.Rat)(div.(*slip.Ratio)))
				r = (*slip.Ratio)(zr.Sub((*big.Rat)(tn), &zp))
			} else {
				q = (*slip.Bignum)(&bi)
				r = (*slip.Ratio)(&zr)
			}
		}
	case slip.Complex:
		slip.PanicType("number", tn, "real")
	}
	return slip.Values{q, r}
}

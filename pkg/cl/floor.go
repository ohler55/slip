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
			f := Floor{Function: slip.Function{Name: "floor", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "floor",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: `The number to take the quotient of and then rounded
to the next integer towards negative infinity.`,
				},
				{Name: "&optional"},
				{
					Name: "divisor",
					Type: "real",
					Text: `The number to divide the _number_ by. The default is 1.`,
				},
			},
			Return: "integer,real",
			Text: `__floor__ returns the quotient of the _numbers_ rounded
toward negative infinity as well as the remainder.`,
			Examples: []string{
				"(floor 5.4) => 5, 0.4",
				"(floor 3/2) => 1, 1/2",
				"(floor 5 2) => 2, 1",
			},
		}, &slip.CLPkg)
}

// Floor represents the floor function.
type Floor struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Floor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return floor(s, f, args, depth)
}

func floor(s *slip.Scope, f slip.Object, args slip.List, depth int) slip.Values {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	num := args[0]
	if _, ok := num.(slip.Number); !ok {
		slip.TypePanic(s, depth, "number", num, "real")
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
			if r.(slip.Fixnum) < 0 {
				q = q.(slip.Fixnum) - slip.Fixnum(1)
				r = r.(slip.Fixnum) + div.(slip.Fixnum)
			}
		} else if r.(slip.Fixnum) < 0 {
			q = q.(slip.Fixnum) + slip.Fixnum(1)
			r = r.(slip.Fixnum) - div.(slip.Fixnum)
		}
	case slip.SingleFloat:
		q = tn / div.(slip.SingleFloat)
		q = slip.Fixnum(math.Floor(float64(q.(slip.SingleFloat))))
		r = tn - slip.SingleFloat(q.(slip.Fixnum))*div.(slip.SingleFloat)
	case slip.DoubleFloat:
		q = tn / div.(slip.DoubleFloat)
		q = slip.Fixnum(math.Floor(float64(q.(slip.DoubleFloat))))
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
			var (
				zq big.Float
				zp big.Float
				zr big.Float
			)
			_ = zq.SetInt(bi)
			d := (*big.Float)(div.(*slip.LongFloat))
			q = (*slip.Bignum)(bi)
			_ = zp.Mul(&zq, d)
			r = (*slip.LongFloat)(zr.Sub((*big.Float)(tn), &zp))
		case big.Above:
			var (
				zq big.Float
				zp big.Float
				zr big.Float
			)
			d := (*big.Float)(div.(*slip.LongFloat))
			bi = bi.Sub(bi, big.NewInt(1))
			_ = quo.SetInt(bi)
			q = (*slip.Bignum)(bi)
			_ = zq.SetInt(bi)
			_ = zp.Mul(&quo, d)
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
		case 1:
			if d.Sign() == 1 {
				q = (*slip.Bignum)(&zq)
				r = (*slip.Bignum)(&zr)
			} else {
				_ = zq.Sub(&zq, big.NewInt(1))
				q = (*slip.Bignum)(&zq)
				var zp big.Int
				_ = zp.Mul(&zq, d)
				r = (*slip.Bignum)(zr.Sub((*big.Int)(tn), &zp))
			}
		case -1:
			if d.Sign() == 1 {
				_ = zq.Sub(&zq, big.NewInt(1))
				q = (*slip.Bignum)(&zq)
				var zp big.Int
				_ = zp.Mul(&zq, d)
				r = (*slip.Bignum)(zr.Sub((*big.Int)(tn), &zp))
			} else {
				q = (*slip.Bignum)(&zq)
				var zp big.Int
				_ = zp.Mul(&zq, d)
				r = (*slip.Bignum)(zr.Sub((*big.Int)(tn), &zp))
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
		d := (*big.Rat)(div.(*slip.Ratio))
		_ = zq.Quo((*big.Rat)(tn), d)
		_ = bi.Quo(zq.Num(), zq.Denom())
		_ = zb.SetInt(&bi)
		_ = zp.Mul(&zb, d)
		_ = zr.Sub((*big.Rat)(tn), &zp)
		switch zr.Sign() {
		case 0:
			q = (*slip.Bignum)(&bi)
			r = slip.Fixnum(0)
		case 1:
			if d.Sign() == 1 {
				q = (*slip.Bignum)(&bi)
				r = (*slip.Ratio)(&zr)
			} else {
				_ = bi.Sub(&bi, big.NewInt(1))
				q = (*slip.Bignum)(&bi)
				_ = zb.SetInt(&bi)
				_ = zp.Mul(&zb, d)
				_ = zr.Sub((*big.Rat)(tn), &zp)
				r = (*slip.Ratio)(&zr)
			}
		case -1:
			if d.Sign() == 1 {
				q = (*slip.Bignum)(bi.Sub(&bi, big.NewInt(1)))
				_ = zb.SetInt(&bi)
				_ = zp.Mul(&zb, d)
				r = (*slip.Ratio)(zr.Sub((*big.Rat)(tn), &zp))
			} else {
				q = (*slip.Bignum)(&bi)
				r = (*slip.Ratio)(&zr)
			}
		}
	case slip.Complex:
		slip.TypePanic(s, depth, "number", tn, "real")
	}
	return slip.Values{q, r}
}

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
			f := Round{Function: slip.Function{Name: "round", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "round",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: `The number to take the quotient of and then rounded
to the next integer towards zero.`,
				},
				{Name: "&optional"},
				{
					Name: "divisor",
					Type: "real",
					Text: `The number to divide the _number_ by. The default is 1.`,
				},
			},
			Return: "integer,real",
			Text: `__round__ returns the quotient of the _numbers_ rounded
toward zero as well as the remainder.`,
			Examples: []string{
				"(round 5.4) => 6, -0.6",
				"(round 3/2) => 2, -1/2",
				"(round 5 2) => 3, -1",
			},
		}, &slip.CLPkg)
}

// Round represents the round function.
type Round struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Round) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return round(f, args)
}

func round(f slip.Object, args slip.List) slip.Values {
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
	num, div = normalizeNumber(num, div)

	switch tn := num.(type) {
	case slip.Fixnum:
		d := div.(slip.Fixnum)
		q = tn / d
		r = tn - q.(slip.Fixnum)*d
		if r == slip.Fixnum(0) {
			break
		}
		ns := tn < slip.Fixnum(0)
		if ns {
			tn = -tn
		}
		ds := d < slip.Fixnum(0)
		if ds {
			d = -d
		}
		q = tn / d
		r = tn - q.(slip.Fixnum)*d
		dif := r.(slip.Fixnum) * 2
		if dif == d && q.(slip.Fixnum)%2 != 0 {
			q = q.(slip.Fixnum) + 1
			r = tn - q.(slip.Fixnum)*d
		}
		if ns {
			r = -r.(slip.Fixnum)
			if !ds {
				q = -q.(slip.Fixnum)
			}
		} else if ds {
			q = -q.(slip.Fixnum)
		}
	case slip.SingleFloat:
		q = tn / div.(slip.SingleFloat)
		q = slip.Fixnum(math.RoundToEven(float64(q.(slip.SingleFloat))))
		r = tn - slip.SingleFloat(q.(slip.Fixnum))*div.(slip.SingleFloat)
	case slip.DoubleFloat:
		q = tn / div.(slip.DoubleFloat)
		q = slip.Fixnum(math.RoundToEven(float64(q.(slip.DoubleFloat))))
		r = tn - slip.DoubleFloat(q.(slip.Fixnum))*div.(slip.DoubleFloat)
	case *slip.LongFloat:
		syncFloatPrec(tn, div.(*slip.LongFloat))
		var zq big.Float
		_ = zq.Quo((*big.Float)(tn), (*big.Float)(div.(*slip.LongFloat)))
		bi, acc := zq.Int(nil)
		if acc == big.Exact {
			q = (*slip.Bignum)(bi)
			r = (*slip.LongFloat)(big.NewFloat(0.0))
			break
		}
		zn := (*big.Float)(tn)
		zd := (*big.Float)(div.(*slip.LongFloat))
		ns := zn.Sign()
		if ns < 0 {
			zn = zn.Abs(zn)
		}
		ds := zd.Sign()
		if ds < 0 {
			zd = zd.Abs(zd)
		}
		_ = zq.Quo(zn, zd)
		var (
			zp big.Float
			zr big.Float
		)
		bi, _ = zq.Int(nil)
		_ = zq.SetInt(bi)
		_ = zp.Mul(&zq, zd)
		_ = zr.Sub(zn, &zp)
		_ = zp.Mul(&zr, big.NewFloat(2.0))
		switch zp.Cmp(zd) {
		case 0:
			if bi.Bit(0) != 0 {
				bi = bi.Add(bi, big.NewInt(1))
				_ = zq.SetInt(bi)
				_ = zp.Mul(&zq, zd)
				_ = zr.Sub(zn, &zp)
			}
		case -1:
			// ok as is
		case 1:
			bi = bi.Add(bi, big.NewInt(1))
			_ = zq.SetInt(bi)
			_ = zp.Mul(&zq, zd)
			_ = zr.Sub(zn, &zp)
		}
		if ns < 0 {
			_ = zr.Neg(&zr)
			if 0 < ds {
				_ = bi.Neg(bi)
			}
		} else if ds < 0 {
			_ = bi.Neg(bi)
		}
		q = (*slip.Bignum)(bi)
		r = (*slip.LongFloat)(&zr)
	case *slip.Bignum:
		var (
			zp big.Int
			zr big.Int
			zq big.Int
		)
		zn := (*big.Int)(tn)
		zd := (*big.Int)(div.(*slip.Bignum))
		ns := zn.Sign()
		if ns < 0 {
			zn = zn.Abs(zn)
		}
		ds := zd.Sign()
		if ds < 0 {
			zd = zd.Abs(zd)
		}
		_, _ = zq.QuoRem(zn, zd, &zr)
		_ = zp.Mul(&zq, zd)
		_ = zr.Sub(zn, &zp)
		_ = zp.Mul(&zr, big.NewInt(2))
		switch zp.Cmp(zd) {
		case 0:
			if zq.Bit(0) != 0 {
				_ = zq.Add(&zq, big.NewInt(1))
				_ = zp.Mul(&zq, zd)
				_ = zr.Sub(zn, &zp)
			}
		case -1:
			// ok as is
		case 1:
			_ = zq.Add(&zq, big.NewInt(1))
			_ = zp.Mul(&zq, zd)
			_ = zr.Sub(zn, &zp)
		}
		if ns < 0 {
			_ = zr.Neg(&zr)
			if 0 < ds {
				_ = zq.Neg(&zq)
			}
		} else if ds < 0 {
			_ = zq.Neg(&zq)
		}
		q = (*slip.Bignum)(&zq)
		r = (*slip.Bignum)(&zr)
	case *slip.Ratio:
		var (
			zp big.Rat
			zr big.Rat
			zq big.Rat
			bi big.Int
			zb big.Rat
		)
		zn := (*big.Rat)(tn)
		zd := (*big.Rat)(div.(*slip.Ratio))
		ns := zn.Sign()
		if ns < 0 {
			zn = zn.Abs(zn)
		}
		ds := zd.Sign()
		if ds < 0 {
			zd = zd.Abs(zd)
		}
		_ = zq.Quo(zn, zd)
		_ = bi.Quo(zq.Num(), zq.Denom())
		_ = zb.SetInt(&bi)
		_ = zp.Mul(&zb, zd)
		_ = zr.Sub(zn, &zp)
		_ = zp.Mul(&zr, big.NewRat(2, 1))
		switch zp.Cmp(zd) {
		case 0:
			if bi.Bit(0) != 0 {
				_ = bi.Add(&bi, big.NewInt(1))
				_ = zq.SetInt(&bi)
				_ = zp.Mul(&zq, zd)
				_ = zr.Sub(zn, &zp)
			}
		case -1:
			// ok as is

		case 1:
			_ = bi.Add(&bi, big.NewInt(1))
			_ = zq.SetInt(&bi)
			_ = zp.Mul(&zq, zd)
			_ = zr.Sub(zn, &zp)
		}
		if ns < 0 {
			_ = zr.Neg(&zr)
			if 0 < ds {
				_ = bi.Neg(&bi)
			}
		} else if ds < 0 {
			_ = bi.Neg(&bi)
		}

		q = (*slip.Bignum)(&bi)
		r = (*slip.Ratio)(&zr)
	case slip.Complex:
		slip.PanicType("number", tn, "real")
	}
	return slip.Values{q, r}
}

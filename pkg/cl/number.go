// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

// v1 should be the derived number that has already been vetted so there is no
// reason to check for a default and panic.
func normalizeNumber(v0, v1 slip.Object) (n0, n1 slip.Object) {
	// Precedence (lowest to highest) fixnum, bignum, ratio, single-float,
	// double-float, long-float, complex.
top:
	switch t0 := v0.(type) {
	case slip.Fixnum:
		n1 = v1
		switch t1 := v1.(type) {
		case slip.Fixnum:
			n0 = t0
		case slip.Octet:
			n0 = t0
			n1 = slip.Fixnum(t1)
		case slip.SingleFloat:
			n0 = slip.SingleFloat(t0)
		case slip.DoubleFloat:
			n0 = slip.DoubleFloat(t0)
		case *slip.LongFloat:
			n0 = (*slip.LongFloat)(big.NewFloat(float64(t0)))
		case *slip.Bignum:
			n0 = (*slip.Bignum)(big.NewInt(int64(t0)))
		case *slip.Ratio:
			n0 = (*slip.Ratio)(big.NewRat(int64(t0), 1))
		case slip.Complex:
			n0 = slip.Complex(complex(float64(t0), 0.0))
		}
	case slip.Octet:
		v0 = slip.Fixnum(t0)
		goto top
	case slip.SingleFloat:
		switch t1 := v1.(type) {
		case slip.Fixnum:
			n0 = t0
			n1 = slip.SingleFloat(t1)
		case slip.SingleFloat:
			n0 = t0
			n1 = t1
		case slip.DoubleFloat:
			n0 = slip.DoubleFloat(t0)
			n1 = t1
		case *slip.LongFloat:
			n0 = (*slip.LongFloat)(big.NewFloat(float64(t0)))
			n1 = t1
		case *slip.Bignum:
			n0 = t0
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t1)).Float64()
			n1 = (slip.SingleFloat)(f)
		case *slip.Ratio:
			n0 = t0
			n1 = slip.SingleFloat(t1.RealValue())
		case slip.Complex:
			n0 = slip.Complex(complex(float64(t0), 0.0))
			n1 = t1
		}
	case slip.DoubleFloat:
		switch t1 := v1.(type) {
		case slip.Fixnum:
			n0 = t0
			n1 = slip.DoubleFloat(t1)
		case slip.SingleFloat:
			n0 = t0
			n1 = slip.DoubleFloat(t1)
		case slip.DoubleFloat:
			n0 = t0
			n1 = t1
		case *slip.LongFloat:
			n0 = (*slip.LongFloat)(big.NewFloat(float64(t0)))
			n1 = t1
		case *slip.Bignum:
			n0 = t0
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t1)).Float64()
			n1 = (slip.DoubleFloat)(f)
		case *slip.Ratio:
			n0 = t0
			n1 = slip.DoubleFloat(t1.RealValue())
		case slip.Complex:
			n0 = slip.Complex(complex(float64(t0), 0.0))
			n1 = t1
		}
	case *slip.LongFloat:
		switch t1 := v1.(type) {
		case slip.Fixnum:
			n0 = t0
			var z big.Float
			z.SetInt64(int64(t1))
			n1 = (*slip.LongFloat)(&z)
		case slip.SingleFloat:
			n0 = t0
			n1 = (*slip.LongFloat)(big.NewFloat(float64(t1)))
		case slip.DoubleFloat:
			n0 = t0
			n1 = (*slip.LongFloat)(big.NewFloat(float64(t1)))
		case *slip.LongFloat:
			n0 = t0
			n1 = t1
		case *slip.Bignum:
			n0 = t0
			var z big.Float
			z.SetInt((*big.Int)(t1))
			n1 = (*slip.LongFloat)(&z)
		case *slip.Ratio:
			n0 = t0
			n1 = (*slip.LongFloat)(big.NewFloat(t1.RealValue()))
		case slip.Complex:
			f, _ := (*big.Float)(t0).Float64()
			n0 = slip.Complex(complex(f, 0.0))
			n1 = t1
		}
	case *slip.Bignum:
		switch t1 := v1.(type) {
		case slip.Fixnum:
			n0 = t0
			n1 = (*slip.Bignum)(big.NewInt(int64(t1)))
		case slip.SingleFloat:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t0)).Float64()
			n0 = slip.SingleFloat(f)
			n1 = t1
		case slip.DoubleFloat:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t0)).Float64()
			n0 = slip.DoubleFloat(f)
			n1 = t1
		case *slip.LongFloat:
			var z big.Float
			n0 = (*slip.LongFloat)(z.SetInt((*big.Int)(t0)))
			n1 = t1
		case *slip.Bignum:
			n0 = t0
			n1 = t1
		case *slip.Ratio:
			if (*big.Int)(t0).IsInt64() {
				n0 = (*slip.Ratio)(big.NewRat((*big.Int)(t0).Int64(), 1))
				n1 = t1
			} else {
				var z big.Float
				n0 = (*slip.LongFloat)(z.SetInt((*big.Int)(t0)))
				f, _ := (*big.Rat)(t1).Float64()
				n1 = (*slip.LongFloat)(big.NewFloat(f))
			}
		case slip.Complex:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t0)).Float64()
			n0 = slip.Complex(complex(f, 0.0))
			n1 = t1
		}
	case *slip.Ratio:
		switch t1 := v1.(type) {
		case slip.Fixnum:
			n0 = t0
			n1 = (*slip.Ratio)(big.NewRat(int64(t1), 1))
		case slip.SingleFloat:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = slip.SingleFloat(f)
			n1 = t1
		case slip.DoubleFloat:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = slip.DoubleFloat(f)
			n1 = t1
		case *slip.LongFloat:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = (*slip.LongFloat)(big.NewFloat(f))
			n1 = t1
		case *slip.Bignum:
			if (*big.Int)(t1).IsInt64() {
				n0 = t0
				n1 = (*slip.Ratio)(big.NewRat((*big.Int)(t1).Int64(), 1))
			} else {
				var z big.Float
				n1 = (*slip.LongFloat)(z.SetInt((*big.Int)(t1)))
				prec := z.Prec()
				var z0 big.Float
				z0.SetPrec(prec)
				z0.SetRat((*big.Rat)(t0))
				n0 = (*slip.LongFloat)(&z0)
			}
		case *slip.Ratio:
			n0 = t0
			n1 = t1
		case slip.Complex:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = slip.Complex(complex(f, 0.0))
			n1 = t1
		}
	case slip.Complex:
		n0 = t0
		switch t1 := v1.(type) {
		case slip.Fixnum:
			n1 = slip.Complex(complex(float64(t1), 0.0))
		case slip.SingleFloat:
			n1 = slip.Complex(complex(float64(t1), 0.0))
		case slip.DoubleFloat:
			n1 = slip.Complex(complex(float64(t1), 0.0))
		case *slip.LongFloat:
			f, _ := (*big.Float)(t1).Float64()
			n1 = slip.Complex(complex(f, 0.0))
		case *slip.Bignum:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t1)).Float64()
			n1 = slip.Complex(complex(f, 0.0))
		case *slip.Ratio:
			f, _ := (*big.Rat)(t1).Float64()
			n1 = slip.Complex(complex(f, 0.0))
		case slip.Complex:
			n1 = t1
		}
	default:
		slip.PanicType("numbers", t0, "number")
	}
	return
}

func syncFloatPrec(v0, v1 *slip.LongFloat) {
	p0 := (*big.Float)(v0).Prec()
	p1 := (*big.Float)(v1).Prec()
	// The big.Float adds random digits when increasing precision so
	// increase the hard way by converting to a string and re-parsing.
	if p0 < p1 {
		s := (*big.Float)(v0).Text('e', -1)
		(*big.Float)(v0).SetPrec(p1)
		_, _, _ = (*big.Float)(v0).Parse(s, 10)
	} else if p1 < p0 {
		s := (*big.Float)(v1).Text('e', -1)
		(*big.Float)(v1).SetPrec(p0)
		_, _, _ = (*big.Float)(v1).Parse(s, 10)
	}
}

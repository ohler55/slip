// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import "math/big"

// NormalizeNumber normalizes numbers to the highest precedence where
// precedence is (lowest to highest) fixnum, bignum, ratio, single-float,
// double-float, long-float, complex. signed-byte and unsigned-byte are
// converted to bignum.
func NormalizeNumber(v0, v1 Object) (n0, n1 Object) {
top:
	switch t0 := v0.(type) {
	case Fixnum:
		n1 = v1
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
		case Octet:
			n0 = t0
			n1 = Fixnum(t1)
		case SingleFloat:
			n0 = SingleFloat(t0)
		case DoubleFloat:
			n0 = DoubleFloat(t0)
		case *LongFloat:
			n0 = (*LongFloat)(big.NewFloat(float64(t0)))
		case *Bignum:
			n0 = (*Bignum)(big.NewInt(int64(t0)))
		case *Ratio:
			n0 = (*Ratio)(big.NewRat(int64(t0), 1))
		case Complex:
			n0 = Complex(complex(float64(t0), 0.0))
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = tt1
			case *Bignum:
				n0 = (*Bignum)(big.NewInt(int64(t0)))
				n1 = tt1
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = tt1
			case *Bignum:
				n0 = (*Bignum)(big.NewInt(int64(t0)))
				n1 = tt1
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case Octet:
		v0 = Fixnum(t0)
		goto top
	case SingleFloat:
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
			n1 = SingleFloat(t1)
		case SingleFloat:
			n0 = t0
			n1 = t1
		case DoubleFloat:
			n0 = DoubleFloat(t0)
			n1 = t1
		case *LongFloat:
			n0 = (*LongFloat)(big.NewFloat(float64(t0)))
			n1 = t1
		case *Bignum:
			n0 = t0
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t1)).Float64()
			n1 = (SingleFloat)(f)
		case *Ratio:
			n0 = t0
			n1 = SingleFloat(t1.RealValue())
		case Complex:
			n0 = Complex(complex(float64(t0), 0.0))
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = SingleFloat(tt1)
			case *Bignum:
				n0 = (*LongFloat)(big.NewFloat(float64(t0)))
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = SingleFloat(tt1)
			case *Bignum:
				n0 = (*LongFloat)(big.NewFloat(float64(t0)))
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case DoubleFloat:
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
			n1 = DoubleFloat(t1)
		case SingleFloat:
			n0 = t0
			n1 = DoubleFloat(t1)
		case DoubleFloat:
			n0 = t0
			n1 = t1
		case *LongFloat:
			n0 = (*LongFloat)(big.NewFloat(float64(t0)))
			n1 = t1
		case *Bignum:
			n0 = t0
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t1)).Float64()
			n1 = (DoubleFloat)(f)
		case *Ratio:
			n0 = t0
			n1 = DoubleFloat(t1.RealValue())
		case Complex:
			n0 = Complex(complex(float64(t0), 0.0))
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = DoubleFloat(tt1)
			case *Bignum:
				n0 = (*LongFloat)(big.NewFloat(float64(t0)))
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = DoubleFloat(tt1)
			case *Bignum:
				n0 = (*LongFloat)(big.NewFloat(float64(t0)))
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case *LongFloat:
		n0 = t0
		switch t1 := v1.(type) {
		case Fixnum:
			var z big.Float
			z.SetInt64(int64(t1))
			n1 = (*LongFloat)(&z)
		case SingleFloat:
			n1 = (*LongFloat)(big.NewFloat(float64(t1)))
		case DoubleFloat:
			n1 = (*LongFloat)(big.NewFloat(float64(t1)))
		case *LongFloat:
			n1 = t1
		case *Bignum:
			var z big.Float
			z.SetInt((*big.Int)(t1))
			n1 = (*LongFloat)(&z)
		case *Ratio:
			n1 = (*LongFloat)(big.NewFloat(t1.RealValue()))
		case Complex:
			f, _ := (*big.Float)(t0).Float64()
			n0 = Complex(complex(f, 0.0))
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = (*LongFloat)(big.NewFloat(float64(tt1)))
			case *Bignum:
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = (*LongFloat)(big.NewFloat(float64(tt1)))
			case *Bignum:
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case *Bignum:
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
			n1 = (*Bignum)(big.NewInt(int64(t1)))
		case SingleFloat:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t0)).Float64()
			n0 = SingleFloat(f)
			n1 = t1
		case DoubleFloat:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t0)).Float64()
			n0 = DoubleFloat(f)
			n1 = t1
		case *LongFloat:
			var z big.Float
			n0 = (*LongFloat)(z.SetInt((*big.Int)(t0)))
			n1 = t1
		case *Bignum:
			n0 = t0
			n1 = t1
		case *Ratio:
			if (*big.Int)(t0).IsInt64() {
				n0 = (*Ratio)(big.NewRat((*big.Int)(t0).Int64(), 1))
				n1 = t1
			} else {
				var z big.Float
				n0 = (*LongFloat)(z.SetInt((*big.Int)(t0)))
				f, _ := (*big.Rat)(t1).Float64()
				n1 = (*LongFloat)(big.NewFloat(f))
			}
		case Complex:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t0)).Float64()
			n0 = Complex(complex(f, 0.0))
			n1 = t1
		case *SignedByte:
			n0 = t0
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = (*Bignum)(big.NewInt(int64(tt1)))
			case *Bignum:
				n1 = tt1
			}
		case *UnsignedByte:
			n0 = t0
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = (*Bignum)(big.NewInt(int64(tt1)))
			case *Bignum:
				n1 = tt1
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case *Ratio:
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
			n1 = (*Ratio)(big.NewRat(int64(t1), 1))
		case SingleFloat:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = SingleFloat(f)
			n1 = t1
		case DoubleFloat:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = DoubleFloat(f)
			n1 = t1
		case *LongFloat:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = (*LongFloat)(big.NewFloat(f))
			n1 = t1
		case *Bignum:
			if (*big.Int)(t1).IsInt64() {
				n0 = t0
				n1 = (*Ratio)(big.NewRat((*big.Int)(t1).Int64(), 1))
			} else {
				var z big.Float
				n1 = (*LongFloat)(z.SetInt((*big.Int)(t1)))
				prec := z.Prec()
				var z0 big.Float
				z0.SetPrec(prec)
				z0.SetRat((*big.Rat)(t0))
				n0 = (*LongFloat)(&z0)
			}
		case *Ratio:
			n0 = t0
			n1 = t1
		case Complex:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = Complex(complex(f, 0.0))
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = (*Ratio)(big.NewRat(int64(tt1), 1))
			case *Bignum:
				var z big.Float
				n1 = (*LongFloat)(z.SetInt((*big.Int)(tt1)))
				prec := z.Prec()
				var z0 big.Float
				z0.SetPrec(prec)
				z0.SetRat((*big.Rat)(t0))
				n0 = (*LongFloat)(&z0)
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = (*Ratio)(big.NewRat(int64(tt1), 1))
			case *Bignum:
				var z big.Float
				n1 = (*LongFloat)(z.SetInt((*big.Int)(tt1)))
				prec := z.Prec()
				var z0 big.Float
				z0.SetPrec(prec)
				z0.SetRat((*big.Rat)(t0))
				n0 = (*LongFloat)(&z0)
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case Complex:
		n0 = t0
		switch t1 := v1.(type) {
		case Fixnum:
			n1 = Complex(complex(float64(t1), 0.0))
		case SingleFloat:
			n1 = Complex(complex(float64(t1), 0.0))
		case DoubleFloat:
			n1 = Complex(complex(float64(t1), 0.0))
		case *LongFloat:
			f, _ := (*big.Float)(t1).Float64()
			n1 = Complex(complex(f, 0.0))
		case *Bignum:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t1)).Float64()
			n1 = Complex(complex(f, 0.0))
		case *Ratio:
			f, _ := (*big.Rat)(t1).Float64()
			n1 = Complex(complex(f, 0.0))
		case Complex:
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = Complex(complex(float64(tt1), 0.0))
			case *Bignum:
				var z big.Float
				f, _ := z.SetInt((*big.Int)(tt1)).Float64()
				n1 = Complex(complex(f, 0.0))
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = Complex(complex(float64(tt1), 0.0))
			case *Bignum:
				var z big.Float
				f, _ := z.SetInt((*big.Int)(tt1)).Float64()
				n1 = Complex(complex(f, 0.0))
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case *SignedByte:
		switch tt0 := t0.AsFixOrBig().(type) {
		case Fixnum:
			v0 = tt0
			goto top
		case *Bignum:
			v0 = tt0
			goto top
		}
	case *UnsignedByte: // change to signed-byte
		switch tt0 := t0.AsFixOrBig().(type) {
		case Fixnum:
			v0 = tt0
			goto top
		case *Bignum:
			v0 = tt0
			goto top
		}
	default:
		PanicType("numbers", t0, "number")
	}
	return
}

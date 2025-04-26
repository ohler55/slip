// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"math/big"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

type nnData struct {
	v0     slip.Object
	v1     slip.Object
	result string
}

func testNormalizeNumber(t *testing.T, data []*nnData) {
	for i, d := range data {
		n0, n1 := slip.NormalizeNumber(d.v0, d.v1)
		tt.Equal(t, d.result, fmt.Sprintf("%s (%T), %s (%T)", n0, n0, n1, n1), "%d: %s %s", i, d.v0, d.v1)
	}
}

func TestNormalizeNumberFixnum(t *testing.T) {
	testNormalizeNumber(t, []*nnData{
		{v0: slip.Fixnum(1), v1: slip.Fixnum(2), result: "1 (slip.Fixnum), 2 (slip.Fixnum)"},
		{v0: slip.Fixnum(1), v1: slip.Octet(2), result: "1 (slip.Fixnum), 2 (slip.Fixnum)"},
		{v0: slip.Fixnum(1), v1: slip.NewBignum(2), result: "1 (*slip.Bignum), 2 (*slip.Bignum)"},
		{v0: slip.Fixnum(1), v1: slip.SingleFloat(2.5), result: "1 (slip.SingleFloat), 2.5 (slip.SingleFloat)"},
		{v0: slip.Fixnum(1), v1: slip.DoubleFloat(2.5), result: "1 (slip.DoubleFloat), 2.5 (slip.DoubleFloat)"},
		{v0: slip.Fixnum(1), v1: slip.NewLongFloat(2.5), result: "1 (*slip.LongFloat), 2.5 (*slip.LongFloat)"},
		{v0: slip.Fixnum(1), v1: slip.Complex(1 + 2i), result: "#C(1 0) (slip.Complex), #C(1 2) (slip.Complex)"},
		{v0: slip.Fixnum(1), v1: slip.NewRatio(1, 2), result: "1 (*slip.Ratio), 1/2 (*slip.Ratio)"},
		{v0: slip.Fixnum(1), v1: slip.SignedByteFromInt64(2), result: "1 (slip.Fixnum), 2 (slip.Fixnum)"},
		{
			v0: slip.Fixnum(1),
			v1: &slip.SignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.Bignum), 75557863725914323421186 (*slip.Bignum)",
		},
		{
			v0:     slip.Fixnum(1),
			v1:     &slip.UnsignedByte{Bytes: []byte{0x08, 0x02}},
			result: "1 (slip.Fixnum), 2050 (slip.Fixnum)",
		},
		{
			v0: slip.Fixnum(1),
			v1: &slip.UnsignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.Bignum), 75557863725914323421186 (*slip.Bignum)",
		},
		{v0: slip.Octet(1), v1: slip.Fixnum(2), result: "1 (slip.Fixnum), 2 (slip.Fixnum)"},
	})
	tt.Panic(t, func() { _, _ = slip.NormalizeNumber(slip.Fixnum(1), slip.True) })
}

func TestNormalizeNumberSingleFloat(t *testing.T) {
	testNormalizeNumber(t, []*nnData{
		{v0: slip.SingleFloat(1.5), v1: slip.Fixnum(2), result: "1.5 (slip.SingleFloat), 2 (slip.SingleFloat)"},
		{v0: slip.SingleFloat(1.5), v1: slip.SingleFloat(2.0), result: "1.5 (slip.SingleFloat), 2 (slip.SingleFloat)"},
		{v0: slip.SingleFloat(1), v1: slip.DoubleFloat(2.5), result: "1 (slip.DoubleFloat), 2.5 (slip.DoubleFloat)"},
		{v0: slip.SingleFloat(1), v1: slip.NewLongFloat(2.5), result: "1 (*slip.LongFloat), 2.5 (*slip.LongFloat)"},
		{v0: slip.SingleFloat(1), v1: slip.NewBignum(2), result: "1 (slip.SingleFloat), 2 (slip.SingleFloat)"},
		{v0: slip.SingleFloat(1), v1: slip.Complex(1 + 2i), result: "#C(1 0) (slip.Complex), #C(1 2) (slip.Complex)"},
		{v0: slip.SingleFloat(1), v1: slip.NewRatio(1, 2), result: "1 (slip.SingleFloat), 0.5 (slip.SingleFloat)"},
		{
			v0:     slip.SingleFloat(1),
			v1:     slip.SignedByteFromInt64(2),
			result: "1 (slip.SingleFloat), 2 (slip.SingleFloat)",
		},
		{
			v0: slip.SingleFloat(1),
			v1: &slip.SignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.LongFloat), 7.5557863725914323421186e+22 (*slip.LongFloat)",
		},
		{
			v0:     slip.SingleFloat(1),
			v1:     &slip.UnsignedByte{Bytes: []byte{0x08, 0x02}},
			result: "1 (slip.SingleFloat), 2050 (slip.SingleFloat)",
		},
		{
			v0: slip.SingleFloat(1),
			v1: &slip.UnsignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.LongFloat), 7.5557863725914323421186e+22 (*slip.LongFloat)",
		},
	})
	tt.Panic(t, func() { _, _ = slip.NormalizeNumber(slip.SingleFloat(1), slip.True) })
}

func TestNormalizeNumberDoubleFloat(t *testing.T) {
	testNormalizeNumber(t, []*nnData{
		{v0: slip.DoubleFloat(1.5), v1: slip.Fixnum(2), result: "1.5 (slip.DoubleFloat), 2 (slip.DoubleFloat)"},
		{v0: slip.DoubleFloat(1.5), v1: slip.SingleFloat(2.0), result: "1.5 (slip.DoubleFloat), 2 (slip.DoubleFloat)"},
		{v0: slip.DoubleFloat(1), v1: slip.DoubleFloat(2.5), result: "1 (slip.DoubleFloat), 2.5 (slip.DoubleFloat)"},
		{v0: slip.DoubleFloat(1), v1: slip.NewLongFloat(2.5), result: "1 (*slip.LongFloat), 2.5 (*slip.LongFloat)"},
		{v0: slip.DoubleFloat(1), v1: slip.NewBignum(2), result: "1 (slip.DoubleFloat), 2 (slip.DoubleFloat)"},
		{v0: slip.DoubleFloat(1), v1: slip.Complex(1 + 2i), result: "#C(1 0) (slip.Complex), #C(1 2) (slip.Complex)"},
		{v0: slip.DoubleFloat(1), v1: slip.NewRatio(1, 2), result: "1 (slip.DoubleFloat), 0.5 (slip.DoubleFloat)"},
		{
			v0:     slip.DoubleFloat(1),
			v1:     slip.SignedByteFromInt64(2),
			result: "1 (slip.DoubleFloat), 2 (slip.DoubleFloat)",
		},
		{
			v0: slip.DoubleFloat(1),
			v1: &slip.SignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.LongFloat), 7.5557863725914323421186e+22 (*slip.LongFloat)",
		},
		{
			v0:     slip.DoubleFloat(1),
			v1:     &slip.UnsignedByte{Bytes: []byte{0x08, 0x02}},
			result: "1 (slip.DoubleFloat), 2050 (slip.DoubleFloat)",
		},
		{
			v0: slip.DoubleFloat(1),
			v1: &slip.UnsignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.LongFloat), 7.5557863725914323421186e+22 (*slip.LongFloat)",
		},
	})
	tt.Panic(t, func() { _, _ = slip.NormalizeNumber(slip.DoubleFloat(1), slip.True) })
}

func TestNormalizeNumberLongFloat(t *testing.T) {
	testNormalizeNumber(t, []*nnData{
		{v0: slip.NewLongFloat(1.5), v1: slip.Fixnum(2), result: "1.5 (*slip.LongFloat), 2 (*slip.LongFloat)"},
		{v0: slip.NewLongFloat(1.5), v1: slip.SingleFloat(2.0), result: "1.5 (*slip.LongFloat), 2 (*slip.LongFloat)"},
		{v0: slip.NewLongFloat(1), v1: slip.DoubleFloat(2.5), result: "1 (*slip.LongFloat), 2.5 (*slip.LongFloat)"},
		{v0: slip.NewLongFloat(1), v1: slip.NewLongFloat(2.5), result: "1 (*slip.LongFloat), 2.5 (*slip.LongFloat)"},
		{v0: slip.NewLongFloat(1), v1: slip.NewBignum(2), result: "1 (*slip.LongFloat), 2 (*slip.LongFloat)"},
		{v0: slip.NewLongFloat(1), v1: slip.Complex(1 + 2i), result: "#C(1 0) (slip.Complex), #C(1 2) (slip.Complex)"},
		{v0: slip.NewLongFloat(1), v1: slip.NewRatio(1, 2), result: "1 (*slip.LongFloat), 0.5 (*slip.LongFloat)"},
		{
			v0:     slip.NewLongFloat(1),
			v1:     slip.SignedByteFromInt64(2),
			result: "1 (*slip.LongFloat), 2 (*slip.LongFloat)",
		},
		{
			v0: slip.NewLongFloat(1),
			v1: &slip.SignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.LongFloat), 7.5557863725914323421186e+22 (*slip.LongFloat)",
		},
		{
			v0:     slip.NewLongFloat(1),
			v1:     &slip.UnsignedByte{Bytes: []byte{0x08, 0x02}},
			result: "1 (*slip.LongFloat), 2050 (*slip.LongFloat)",
		},
		{
			v0: slip.NewLongFloat(1),
			v1: &slip.UnsignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.LongFloat), 7.5557863725914323421186e+22 (*slip.LongFloat)",
		},
	})
	tt.Panic(t, func() { _, _ = slip.NormalizeNumber(slip.NewLongFloat(1), slip.True) })
}

func TestNormalizeNumberBignum(t *testing.T) {
	var bi big.Int
	_ = bi.SetBytes([]byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02})

	testNormalizeNumber(t, []*nnData{
		{v0: slip.NewBignum(1), v1: slip.Fixnum(2), result: "1 (*slip.Bignum), 2 (*slip.Bignum)"},
		{v0: slip.NewBignum(1), v1: slip.SingleFloat(2.0), result: "1 (slip.SingleFloat), 2 (slip.SingleFloat)"},
		{v0: slip.NewBignum(1), v1: slip.DoubleFloat(2.5), result: "1 (slip.DoubleFloat), 2.5 (slip.DoubleFloat)"},
		{v0: slip.NewBignum(1), v1: slip.NewLongFloat(2.5), result: "1 (*slip.LongFloat), 2.5 (*slip.LongFloat)"},
		{v0: slip.NewBignum(1), v1: slip.NewBignum(2), result: "1 (*slip.Bignum), 2 (*slip.Bignum)"},
		{v0: slip.NewBignum(1), v1: slip.Complex(1 + 2i), result: "#C(1 0) (slip.Complex), #C(1 2) (slip.Complex)"},
		{v0: slip.NewBignum(1), v1: slip.NewRatio(1, 2), result: "1 (*slip.Ratio), 1/2 (*slip.Ratio)"},
		{
			v0:     (*slip.Bignum)(&bi),
			v1:     slip.NewRatio(1, 2),
			result: "7.5557863725914323421186e+22 (*slip.LongFloat), 0.5 (*slip.LongFloat)",
		},
		{
			v0:     slip.NewBignum(1),
			v1:     slip.SignedByteFromInt64(2),
			result: "1 (*slip.Bignum), 2 (*slip.Bignum)",
		},
		{
			v0: slip.NewBignum(1),
			v1: &slip.SignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.Bignum), 75557863725914323421186 (*slip.Bignum)",
		},
		{
			v0:     slip.NewBignum(1),
			v1:     &slip.UnsignedByte{Bytes: []byte{0x08, 0x02}},
			result: "1 (*slip.Bignum), 2050 (*slip.Bignum)",
		},
		{
			v0: slip.NewBignum(1),
			v1: &slip.UnsignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1 (*slip.Bignum), 75557863725914323421186 (*slip.Bignum)",
		},
	})
	tt.Panic(t, func() { _, _ = slip.NormalizeNumber(slip.NewBignum(1), slip.True) })
}

func TestNormalizeNumberRatio(t *testing.T) {
	var bi big.Int
	_ = bi.SetBytes([]byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02})

	testNormalizeNumber(t, []*nnData{
		{v0: slip.NewRatio(3, 2), v1: slip.Fixnum(2), result: "3/2 (*slip.Ratio), 2 (*slip.Ratio)"},
		{v0: slip.NewRatio(3, 2), v1: slip.SingleFloat(2.0), result: "1.5 (slip.SingleFloat), 2 (slip.SingleFloat)"},
		{v0: slip.NewRatio(3, 2), v1: slip.DoubleFloat(2.5), result: "1.5 (slip.DoubleFloat), 2.5 (slip.DoubleFloat)"},
		{v0: slip.NewRatio(3, 2), v1: slip.NewLongFloat(2.5), result: "1.5 (*slip.LongFloat), 2.5 (*slip.LongFloat)"},
		{v0: slip.NewRatio(3, 2), v1: slip.NewBignum(2), result: "3/2 (*slip.Ratio), 2 (*slip.Ratio)"},
		{v0: slip.NewRatio(3, 2), v1: slip.Complex(1 + 2i), result: "#C(1.5 0) (slip.Complex), #C(1 2) (slip.Complex)"},
		{v0: slip.NewRatio(3, 2), v1: slip.NewRatio(1, 2), result: "3/2 (*slip.Ratio), 1/2 (*slip.Ratio)"},
		{
			v0:     slip.NewRatio(3, 2),
			v1:     (*slip.Bignum)(&bi),
			result: "1.5 (*slip.LongFloat), 7.5557863725914323421186e+22 (*slip.LongFloat)",
		},
		{
			v0:     slip.NewRatio(3, 2),
			v1:     slip.SignedByteFromInt64(2),
			result: "3/2 (*slip.Ratio), 2 (*slip.Ratio)",
		},
		{
			v0: slip.NewRatio(3, 2),
			v1: &slip.SignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1.5 (*slip.LongFloat), 7.5557863725914323421186e+22 (*slip.LongFloat)",
		},
		{
			v0:     slip.NewRatio(3, 2),
			v1:     &slip.UnsignedByte{Bytes: []byte{0x08, 0x02}},
			result: "3/2 (*slip.Ratio), 2050 (*slip.Ratio)",
		},
		{
			v0: slip.NewRatio(3, 2),
			v1: &slip.UnsignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "1.5 (*slip.LongFloat), 7.5557863725914323421186e+22 (*slip.LongFloat)",
		},
	})
	tt.Panic(t, func() { _, _ = slip.NormalizeNumber(slip.NewRatio(3, 2), slip.True) })
}

func TestNormalizeNumberComplex(t *testing.T) {
	var bi big.Int
	_ = bi.SetBytes([]byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02})

	testNormalizeNumber(t, []*nnData{
		{v0: slip.Complex(3 + 2i), v1: slip.Fixnum(2), result: "#C(3 2) (slip.Complex), #C(2 0) (slip.Complex)"},
		{v0: slip.Complex(3 + 2i), v1: slip.SingleFloat(2.0), result: "#C(3 2) (slip.Complex), #C(2 0) (slip.Complex)"},
		{v0: slip.Complex(3 + 2i), v1: slip.DoubleFloat(2.0), result: "#C(3 2) (slip.Complex), #C(2 0) (slip.Complex)"},
		{v0: slip.Complex(3 + 2i), v1: slip.NewLongFloat(2), result: "#C(3 2) (slip.Complex), #C(2 0) (slip.Complex)"},
		{v0: slip.Complex(3 + 2i), v1: slip.NewBignum(2), result: "#C(3 2) (slip.Complex), #C(2 0) (slip.Complex)"},
		{v0: slip.Complex(3 + 2i), v1: slip.Complex(1 + 2i), result: "#C(3 2) (slip.Complex), #C(1 2) (slip.Complex)"},
		{v0: slip.Complex(3 + 2i), v1: slip.NewRatio(1, 2), result: "#C(3 2) (slip.Complex), #C(0.5 0) (slip.Complex)"},
		{
			v0:     slip.Complex(3 + 2i),
			v1:     (*slip.Bignum)(&bi),
			result: "#C(3 2) (slip.Complex), #C(7.555786372591432e+22 0) (slip.Complex)",
		},
		{
			v0:     slip.Complex(3 + 2i),
			v1:     slip.SignedByteFromInt64(2),
			result: "#C(3 2) (slip.Complex), #C(2 0) (slip.Complex)",
		},
		{
			v0: slip.Complex(3 + 2i),
			v1: &slip.SignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "#C(3 2) (slip.Complex), #C(7.555786372591432e+22 0) (slip.Complex)",
		},
		{
			v0:     slip.Complex(3 + 2i),
			v1:     &slip.UnsignedByte{Bytes: []byte{0x08, 0x02}},
			result: "#C(3 2) (slip.Complex), #C(2050 0) (slip.Complex)",
		},
		{
			v0: slip.Complex(3 + 2i),
			v1: &slip.UnsignedByte{
				Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
			},
			result: "#C(3 2) (slip.Complex), #C(7.555786372591432e+22 0) (slip.Complex)",
		},
	})
	tt.Panic(t, func() { _, _ = slip.NormalizeNumber(slip.Complex(3+2i), slip.True) })
}

func TestNormalizeNumberSignedByte(t *testing.T) {
	var bi big.Int
	_ = bi.SetBytes([]byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02})

	testNormalizeNumber(t, []*nnData{
		{v0: slip.SignedByteFromInt64(1), v1: slip.Fixnum(2), result: "1 (slip.Fixnum), 2 (slip.Fixnum)"},
		{
			v0:     slip.SignedByteFromBigInt(&bi),
			v1:     slip.Fixnum(2),
			result: "75557863725914323421186 (*slip.Bignum), 2 (*slip.Bignum)",
		},
	})
}

func TestNormalizeNumberUnsignedByte(t *testing.T) {
	testNormalizeNumber(t, []*nnData{
		{v0: &slip.UnsignedByte{Bytes: []byte{0x01}}, v1: slip.Fixnum(2), result: "1 (slip.Fixnum), 2 (slip.Fixnum)"},
		{
			v0:     &slip.UnsignedByte{Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02}},
			v1:     slip.Fixnum(2),
			result: "75557863725914323421186 (*slip.Bignum), 2 (*slip.Bignum)",
		},
	})
}

func TestNormalizeNumberNotNumber(t *testing.T) {
	tt.Panic(t, func() { _, _ = slip.NormalizeNumber(slip.True, slip.Fixnum(1)) })
}

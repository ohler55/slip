// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBitVector(t *testing.T) {
	bv := slip.BitVector{Bytes: []byte{0xaa}, Size: 5}
	(&sliptest.Object{
		Target:    &bv,
		String:    "#*10101",
		Simple:    "#*10101",
		Hierarchy: "bit-vector.vector.array.sequence.t",
		Equals: []*sliptest.EqTest{
			{Other: &bv, Expect: true},
			{Other: slip.Octets("ABC"), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: &bv,
	}).Test(t)
	tt.Equal(t, slip.BitVectorSymbol, bv.SequenceType())
	tt.Equal(t, slip.BitVectorSymbol, bv.ArrayType())

	bv = slip.BitVector{Bytes: []byte{0xaa, 0xff}, Size: 16}
	tt.Equal(t, "#*1010101011111111", bv.String())
	bv.Size = 15
	tt.Equal(t, "#*101010101111111", bv.String())
	bv.Size = 9
	tt.Equal(t, "#*101010101", bv.String())
	bv.Size = 0
	tt.Equal(t, "#*", bv.String())
}

func TestBitVectorAt(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	tt.Equal(t, true, bv.At(0))
	tt.Equal(t, false, bv.At(1))
	tt.Equal(t, false, bv.At(5))
}

func TestBitVectorSet(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.Set(0, false)
	bv.Set(1, true)
	tt.Equal(t, true, bv.At(1))
	tt.Equal(t, false, bv.At(0))

	tt.Panic(t, func() { bv.Set(5, true) })
}

func TestBitVectorPush(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.Adjustable = true

	bv.Push(slip.Fixnum(0), slip.Bit(1))
	tt.Equal(t, "#*101001", bv.String())

	bv.FillPtr = 2
	bv.Push(slip.Fixnum(0), slip.Bit(1))
	tt.Equal(t, "#*1001", bv.String())
	tt.Equal(t, 4, bv.FillPtr)

	bv.FillPtr = 5
	bv.Push(slip.Fixnum(0), slip.Bit(1))
	tt.Equal(t, "#*1001001", bv.String())
	tt.Equal(t, 7, bv.FillPtr)

	// Cause a bv grow.
	bv.Push(slip.Bit(0), slip.Bit(1))
	tt.Equal(t, "#*100100101", bv.String())
	tt.Equal(t, 9, bv.FillPtr)
	tt.Equal(t, 9, bv.Length())

	tt.Panic(t, func() { bv.Push(slip.Fixnum(2)) })
}

func TestBitVectorPop(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	tt.Equal(t, slip.Bit(0), bv.Pop())
	tt.Equal(t, slip.Bit(1), bv.Pop())
	tt.Equal(t, "#*10", bv.String())

	bv.Push(slip.Bit(0), slip.Bit(1), slip.Bit(0), slip.Bit(1))
	bv.FillPtr = 4
	tt.Equal(t, slip.Bit(1), bv.Pop())
	tt.Equal(t, slip.Bit(0), bv.Pop())
	tt.Equal(t, "#*10", bv.String())
	bv.FillPtr = int(bv.Size)
	tt.Equal(t, "#*100101", bv.String())
}

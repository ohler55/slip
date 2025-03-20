// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBitVector(t *testing.T) {
	bv := slip.BitVector{Bytes: []byte{0xaa}, Len: 5, FillPtr: -1}
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

	bv = slip.BitVector{Bytes: []byte{0xaa, 0xff}, Len: 16, FillPtr: -1}
	tt.Equal(t, "#*1010101011111111", bv.String())
	bv.Len = 15
	tt.Equal(t, "#*101010101111111", bv.String())
	bv.Len = 9
	tt.Equal(t, "#*101010101", bv.String())
	bv.Len = 0
	tt.Equal(t, "#*", bv.String())
}

func TestBitVectorAt(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	tt.Equal(t, true, bv.At(0))
	tt.Equal(t, false, bv.At(1))
	tt.Equal(t, false, bv.At(5))
}

func TestBitVectorPut(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.Put(0, false)
	bv.Put(1, true)
	tt.Equal(t, true, bv.At(1))
	tt.Equal(t, false, bv.At(0))

	tt.Panic(t, func() { bv.Put(5, true) })
}

func TestBitVectorElementType(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	tt.Equal(t, slip.BitSymbol, bv.ElementType())
}

func TestBitVectorRank(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	tt.Equal(t, 1, bv.Rank())
}

func TestBitVectorAdjustable(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	tt.Equal(t, false, bv.Adjustable())
}

func TestBitVectorPush(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.CanAdjust = true

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
	tt.Equal(t, 9, bv.FillPointer())
	tt.Equal(t, 9, bv.Length())
	tt.Equal(t, true, bv.Adjustable())
	tt.Equal(t, slip.BitSymbol, bv.ElementType())
	tt.Equal(t, []int{9}, bv.Dimensions())

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
	bv.FillPtr = int(bv.Len)
	tt.Equal(t, "#*100101", bv.String())
}

func TestBitVectorAsList(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	tt.Equal(t, slip.List{slip.Bit(1), slip.Bit(0), slip.Bit(1), slip.Bit(0)}, bv.AsList())

	bv.FillPtr = 2
	tt.Equal(t, slip.List{slip.Bit(1), slip.Bit(0)}, bv.AsList())

	bv = slip.ReadBitVector([]byte("1010110011"))
	tt.Equal(t, "(1 0 1 0 1 1 0 0 1 1)", bv.AsList().String())
}

func TestBitVectorAdjustInitialElement(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.CanAdjust = true

	v2 := bv.Adjust([]int{6}, slip.BitSymbol, slip.Fixnum(1), nil, 6)
	tt.Equal(t, "#*101011", slip.ObjectString(bv))
	tt.Equal(t, bv, v2)

	tt.Panic(t, func() { bv.Adjust([]int{5}, slip.TrueSymbol, slip.Bit(1), nil, 4) })
	tt.Panic(t, func() { bv.Adjust([]int{5}, slip.BitSymbol, slip.True, nil, 4) })
}

func TestBitVectorAdjustInitialContent(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.CanAdjust = true

	v2 := bv.Adjust([]int{6}, slip.BitSymbol, slip.Bit(0),
		slip.List{slip.Bit(0), slip.Fixnum(1), slip.Bit(0), slip.Bit(1), slip.Bit(0), slip.Bit(1)},
		6)
	tt.Equal(t, "#*010101", slip.ObjectString(bv))
	tt.Equal(t, bv, v2)

	tt.Panic(t, func() { bv.Adjust([]int{5}, slip.BitSymbol, slip.Bit(0), slip.List{slip.Bit(0), slip.Bit(0)}, 4) })
	tt.Panic(t, func() { bv.Adjust([]int{2}, slip.BitSymbol, slip.Bit(0), slip.List{slip.Bit(0), slip.Fixnum(3)}, 2) })
}

func TestBitVectorAdjustDimensions(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.CanAdjust = true

	bv.Adjust([]int{2}, slip.BitSymbol, slip.Bit(0), nil, 2)
	tt.Equal(t, "#*10", slip.ObjectString(bv))

	tt.Panic(t, func() { bv.Adjust([]int{5, 6}, slip.BitSymbol, slip.Bit(1), nil, 4) })
}

func TestBitVectorAdjustDup(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.CanAdjust = false

	v2 := bv.Adjust([]int{6}, slip.BitSymbol, slip.Fixnum(1), nil, 6)
	tt.Equal(t, "#*1010", slip.ObjectString(bv))
	tt.Equal(t, "#*101011", slip.ObjectString(v2))
	tt.NotEqual(t, bv, v2)
}

func TestBitVectorGet(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	tt.Equal(t, slip.Bit(0), bv.Get(1))
	tt.Equal(t, slip.Bit(1), bv.Get(2))
	tt.Panic(t, func() { _ = bv.Get(1, 2) })
	tt.Panic(t, func() { _ = bv.Get(5) })
}

func TestBitVectorSet(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.Set(slip.Fixnum(1), 1)
	tt.Equal(t, slip.Bit(1), bv.Get(1))
	bv.Set(slip.Fixnum(0), 1)
	tt.Equal(t, slip.Bit(0), bv.Get(1))
	tt.Panic(t, func() { bv.Set(slip.Octet(1), 1, 2) })
	tt.Panic(t, func() { bv.Set(slip.Octet(1), 5) })
	tt.Panic(t, func() { bv.Set(slip.Fixnum(3), 1) })
}

func TestBitVectorDuplicate(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.FillPtr = 4
	bv.CanAdjust = true

	dup := bv.Duplicate()

	tt.Equal(t, bv.FillPointer(), dup.FillPointer())
	tt.Equal(t, true, dup.CanAdjust)
	tt.Equal(t, 4, dup.Length())
	tt.Equal(t, "#*1010", dup.String())
}

func TestBitVectorReverse(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.Reverse()
	tt.Equal(t, "#*0101", bv.String())

	bv = slip.ReadBitVector([]byte("101011001010"))
	bv.Reverse()
	tt.Equal(t, "#*010100110101", bv.String())
}

func TestBitVectorMajorIndex(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	tt.Equal(t, 0, bv.MajorIndex(0))
	tt.Equal(t, 1, bv.MajorIndex(1))

	tt.Panic(t, func() { _ = bv.MajorIndex(1, 2) })
	tt.Panic(t, func() { _ = bv.MajorIndex(4) })
}

func TestBitVectorMajorSet(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.MajorSet(0, slip.Bit(0))
	bv.MajorSet(1, slip.Bit(1))
	tt.Equal(t, "#*0110", bv.String())

	tt.Panic(t, func() { bv.MajorSet(4, slip.Bit(1)) })
}

func TestBitVectorSetFillPointer(t *testing.T) {
	bv := slip.ReadBitVector([]byte("1010"))
	bv.SetFillPointer(2)
	tt.Equal(t, "#*10", bv.String())

	tt.Panic(t, func() { bv.SetFillPointer(4) })
}

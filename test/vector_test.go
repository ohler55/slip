// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestVectorObject(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewVector(
			slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3), nil}, slip.TrueSymbol, true),
		String:    "#(1 2 3 nil)",
		Simple:    []any{int64(1), int64(2), int64(3), nil},
		Hierarchy: "vector.array.sequence.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.NewVector(
				slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3), nil}, slip.TrueSymbol, true),
				Expect: true},
			{Other: slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), nil}, slip.TrueSymbol, true),
				Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3), nil}, slip.TrueSymbol, true),
		Selfies: []func() slip.Symbol{
			(&slip.Vector{}).SequenceType,
			(&slip.Vector{}).ArrayType,
		},
	}).Test(t)
	tt.Equal(t, 2, slip.NewVector(slip.List{slip.True, nil}, slip.TrueSymbol, true).Length())
}

func TestVectorGet(t *testing.T) {
	v := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, slip.TrueSymbol, true)
	val := v.Get(1)
	tt.Equal(t, slip.Fixnum(2), val)
}

func TestVectorSet(t *testing.T) {
	v := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, slip.TrueSymbol, true)
	v.Set(slip.Fixnum(42), 1)
	val := v.Get(1)
	tt.Equal(t, slip.Fixnum(42), val)
}

func TestVectorEqual(t *testing.T) {
	v := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, slip.TrueSymbol, true)
	v2 := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, slip.TrueSymbol, true)
	v3 := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(1), slip.Fixnum(3)}, slip.TrueSymbol, true)
	tt.Equal(t, true, v.Equal(v2))
	tt.Equal(t, false, v.Equal(v3))
}

func TestVectorAsList(t *testing.T) {
	v := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, slip.TrueSymbol, true)
	tt.Equal(t, "(1 2 3)", slip.ObjectString(v.AsList()))

	v.FillPtr = 2
	tt.Equal(t, "(1 2)", slip.ObjectString(v.AsList()))
}

func TestVectorPopFillPtr(t *testing.T) {
	v := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, slip.TrueSymbol, true)
	v.FillPtr = 2
	val := v.Pop()
	tt.Equal(t, slip.Fixnum(2), val)
	tt.Equal(t, "#(1)", slip.ObjectString(v))
	v.FillPtr = 3
	tt.Equal(t, "#(1 2 3)", slip.ObjectString(v))
}

func TestVectorPopNoFillPtr(t *testing.T) {
	v := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, slip.TrueSymbol, true)
	val := v.Pop()
	tt.Equal(t, slip.Fixnum(3), val)
	tt.Equal(t, "#(1 2)", slip.ObjectString(v))
	tt.Equal(t, slip.Fixnum(2), v.Pop())
	tt.Equal(t, slip.Fixnum(1), v.Pop())
	tt.Equal(t, nil, v.Pop())
}

func TestVectorPushFillPtr(t *testing.T) {
	v := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, slip.TrueSymbol, true)
	v.FillPtr = 1
	val := v.Push(slip.Fixnum(4))
	tt.Equal(t, 2, val)
	tt.Equal(t, "#(1 4)", slip.ObjectString(v))
	v.FillPtr = 3
	tt.Equal(t, "#(1 4 3)", slip.ObjectString(v))
	val = v.Push(slip.Fixnum(5))
	tt.Equal(t, 4, val)
	tt.Equal(t, "#(1 4 3 5)", slip.ObjectString(v))
}

func TestVectorPushNoFillPtr(t *testing.T) {
	v := slip.NewVector(slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, slip.TrueSymbol, true)
	val := v.Push(slip.Fixnum(4))
	tt.Equal(t, 4, val)
	tt.Equal(t, "#(1 2 3 4)", slip.ObjectString(v))
}

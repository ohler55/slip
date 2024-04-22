// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArray(t *testing.T) {
	(&sliptest.Object{
		Target: testArray(),
		String: "#<(ARRAY T (2 3 4))>",
		Simple: sen.MustParse([]byte(`
[
  [[0 1 2 3] [4 5 6 7] [8 9 10 11]]
  [[12 13 14 15] [16 17 18 19] [20 21 22 23]]
]`)),
		Hierarchy: "array.t",
		Equals: []*sliptest.EqTest{
			{Other: testArray(), Expect: true},
			{Other: slip.NewArray([]int{1, 2}, slip.TrueSymbol, nil, nil, true), Expect: false},
			{Other: slip.NewArray([]int{3, 2, 4}, slip.TrueSymbol, nil, nil, true), Expect: false},
			{Other: slip.NewArray([]int{2, 3, 4}, slip.TrueSymbol, nil, nil, true), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: testArray(),
	}).Test(t)
}

func TestArrayGet(t *testing.T) {
	a := testArray()
	val := a.Get(1, 1, 1)
	tt.Equal(t, slip.Fixnum(17), val)
	tt.Panic(t, func() { _ = a.Get(1, 1) })
	tt.Panic(t, func() { _ = a.Get(1, -1, 1) })
}

func TestArraySet(t *testing.T) {
	a := testArray()
	a.Set(slip.Fixnum(42), 1, 1, 1)
	val := a.Get(1, 1, 1)
	tt.Equal(t, slip.Fixnum(42), val)
	tt.Panic(t, func() { a.Set(nil, 1, 1) })
	tt.Panic(t, func() { a.Set(nil, 1, -1, 1) })
}

func TestArraySetAll(t *testing.T) {
	content := testArray().AsList()
	a := slip.NewArray([]int{2, 3, 4}, slip.TrueSymbol, nil, nil, true)
	a.SetAll(content)
	tt.Equal(t, "(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((12 13 14 15) (16 17 18 19) (20 21 22 23)))",
		a.AsList().String())

	a = slip.NewArray([]int{2, 2}, slip.TrueSymbol, nil, nil, true)
	tt.Panic(t, func() { a.SetAll(slip.List{slip.List{nil}, slip.List{nil}}) })
	tt.Panic(t, func() { a.SetAll(slip.List{nil, nil}) })
}

func TestArraySetBadData(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("#2A(nil (1 2 3)(4 5 6))") })
}

func TestArrayMisc(t *testing.T) {
	a := testArray()
	tt.Equal(t, 24, a.Size())
	tt.Equal(t, []int{2, 3, 4}, a.Dimensions())
	tt.Equal(t, 3, a.Rank())
	tt.Equal(t, true, a.Adjustable())
	tt.Equal(t, slip.TrueSymbol, a.ElementType())
	tt.Equal(t, []interface{}{nil}, slip.NewArray([]int{1}, slip.TrueSymbol, nil, nil, true).Simplify())
}

func TestArrayAdjustShrink(t *testing.T) {
	a := testArray()
	a.Adjust([]int{1, 2, 3}, slip.TrueSymbol, nil, nil)
	tt.Equal(t, "(((0 1 2) (4 5 6)))",
		a.AsList().String())
}

func TestArrayAdjustExpand(t *testing.T) {
	a := testArray()
	_ = a.Adjust([]int{1, 2, 3}, slip.TrueSymbol, nil, nil) // shrink first

	a.Adjust([]int{2, 3, 4}, slip.TrueSymbol, slip.Fixnum(99), nil)
	tt.Equal(t, "(((0 1 2 99) (4 5 6 99) (99 99 99 99)) ((99 99 99 99) (99 99 99 99) (99 99 99 99)))",
		a.AsList().String())
}

func TestArrayAdjustNotAdjustable(t *testing.T) {
	a := slip.NewArray([]int{1, 2}, slip.TrueSymbol, nil, slip.List{slip.List{nil, nil}}, false)
	a2 := a.Adjust([]int{1, 2}, slip.FixnumSymbol, nil, nil)
	tt.Equal(t, slip.TrueSymbol, a.ElementType())
	tt.Equal(t, slip.FixnumSymbol, a2.ElementType())
}

func TestArrayAdjustTypeCheck(t *testing.T) {
	a := slip.NewArray([]int{1, 2}, slip.TrueSymbol, nil, slip.List{slip.List{slip.Fixnum(1), slip.Fixnum(2)}}, true)
	a2 := a.Adjust([]int{1, 2}, slip.FixnumSymbol, nil, nil)
	tt.Equal(t, slip.FixnumSymbol, a.ElementType())
	tt.Equal(t, slip.FixnumSymbol, a2.ElementType())
}

func TestArrayAdjustInitContents(t *testing.T) {
	a := slip.NewArray([]int{1, 2}, slip.TrueSymbol, nil, slip.List{slip.List{slip.Fixnum(1), slip.Fixnum(2)}}, true)
	_ = a.Adjust([]int{1, 2}, slip.FixnumSymbol, nil, slip.List{slip.List{slip.Fixnum(3), slip.Fixnum(4)}})
	tt.Equal(t, "((3 4))", a.AsList().String())

	tt.Panic(t, func() {
		_ = a.Adjust([]int{1, 2}, slip.FixnumSymbol, nil, slip.True)
	})
}

func TestArrayAdjustBadDims(t *testing.T) {
	a := slip.NewArray([]int{1, 2}, slip.TrueSymbol, nil, slip.List{slip.List{nil, nil}}, true)
	tt.Panic(t, func() { _ = a.Adjust([]int{1, 2, 3}, slip.TrueSymbol, nil, nil) })
}

func TestArrayContent(t *testing.T) {
	a := slip.NewArray([]int{1, 2}, slip.TrueSymbol, nil, slip.List{slip.List{nil, nil}}, true)
	tt.Equal(t, "((nil nil))", a.AsList().String())

	tt.Panic(t, func() { _ = slip.NewArray([]int{1, 2}, slip.TrueSymbol, nil, slip.TrueSymbol, true) })
	tt.Panic(t, func() {
		_ = slip.NewArray([]int{1, 2}, slip.FixnumSymbol, nil, slip.List{slip.List{slip.Symbol("x"), nil}}, true)
	})
}

func testArray() *slip.Array {
	a := slip.NewArray([]int{2, 3, 4}, slip.TrueSymbol, nil, nil, true)
	v := 0
	for i := 0; i < 2; i++ {
		for j := 0; j < 3; j++ {
			for k := 0; k < 4; k++ {
				a.Set(slip.Fixnum(v), i, j, k)
				v++
			}
		}
	}
	return a
}

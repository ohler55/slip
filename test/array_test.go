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
			{Other: slip.NewArray(nil, 1, 2), Expect: false},
			{Other: slip.NewArray(nil, 3, 2, 4), Expect: false},
			{Other: slip.NewArray(nil, 2, 3, 4), Expect: false},
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
	a := slip.NewArray(nil, 2, 3, 4)
	a.SetAll(content)
	tt.Equal(t, "(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((12 13 14 15) (16 17 18 19) (20 21 22 23)))",
		a.AsList().String())

	a = slip.NewArray(nil, 2, 2)
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
	tt.Equal(t, []interface{}{nil}, slip.NewArray(nil, 1).Simplify())
}

func testArray() *slip.Array {
	a := slip.NewArray(nil, 2, 3, 4)
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

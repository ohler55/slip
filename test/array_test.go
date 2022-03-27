// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArray(t *testing.T) {
	// TBD need other variations on the test array for Equal test
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
			{Other: slip.True, Expect: false},
		},
		Eval: testArray(),
	}).Test(t)
}

func testArray() *slip.Array {
	a := slip.NewArray(2, 3, 4)
	v := 0
	for i := 0; i < 2; i++ {
		for j := 0; j < 3; j++ {
			for k := 0; k < 4; k++ {
				a.Set(slip.Fixnum(v), i, j, k)
				v++
			}
		}
	}
	fmt.Printf("*** array: %v\n", a.AsList())
	return a
}

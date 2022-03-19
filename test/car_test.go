// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCarEmpty(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("car", slip.List{nil}),
		String:    "(car nil)",
		Simple:    []interface{}{"car", nil},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: nil,
	}).Test(t)
}

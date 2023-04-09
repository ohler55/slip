// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestReturnResult(t *testing.T) {
	rr := cl.ReturnResult{Tag: slip.Symbol("tug"), Result: slip.Fixnum(3)}
	(&sliptest.Object{
		Target:    &rr,
		String:    "#<return-result tug>",
		Simple:    map[string]any{"tag": "tug", "result": 3},
		Hierarchy: "t",
		Equals: []*sliptest.EqTest{
			{Other: &rr, Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
		},
		Eval: &rr,
	}).Test(t)
}

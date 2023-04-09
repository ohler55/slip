// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestGoTo(t *testing.T) {
	gt := cl.GoTo{Tag: slip.Symbol("tug")}
	(&sliptest.Object{
		Target:    &gt,
		String:    "#<go tug>",
		Simple:    map[string]any{"tag": "tug"},
		Hierarchy: "t",
		Equals: []*sliptest.EqTest{
			{Other: &gt, Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
		},
		Eval: &gt,
	}).Test(t)
}

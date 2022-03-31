// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestListEmpty(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("list", slip.List{}),
		String:    "(list)",
		Simple:    []interface{}{"list"},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: slip.List{},
	}).Test(t)
}

func TestListBasic(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("list", slip.List{slip.Fixnum(2), slip.Fixnum(1)}),
		String:    "(list 1 2)",
		Simple:    []interface{}{"list", 1, 2},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: slip.List{slip.Fixnum(2), slip.Fixnum(1)},
	}).Test(t)
}

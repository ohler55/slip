// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestListEmpty(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("list", slip.List{}),
		String:    "(list)",
		Simple:    []interface{}{"list"},
		Hierarchy: "built-in.function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: slip.List{},
	}).Test(t)
}

func TestListBasic(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("list", slip.List{slip.Fixnum(1), slip.Fixnum(2)}),
		String:    "(list 1 2)",
		Simple:    []interface{}{"list", 1, 2},
		Hierarchy: "built-in.function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: slip.List{slip.Fixnum(1), slip.Fixnum(2)},
	}).Test(t)
}

func TestListValues(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`(list 'a (values 'b 'c) 'd)`, scope)
	tt.Equal(t, "(a b d)", slip.ObjectString(code.Eval(scope, nil)))
}

// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
	"github.com/stretchr/testify/require"
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

func TestListValues(t *testing.T) {
	code := slip.ReadString(`(list 'a (values 'b 'c) 'd)`)
	scope := slip.NewScope()
	require.Equal(t, "(a b d)", slip.ObjectString(code.Eval(scope)))
}

// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestConsCons(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("cons", slip.List{slip.Fixnum(2), slip.Fixnum(1)}),
		String:    "(cons 1 2)",
		Simple:    []interface{}{"cons", 1, 2},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: slip.List{slip.Tail{Value: slip.Fixnum(2)}, slip.Fixnum(1)},
	}).Test(t)
}

func TestConsNil(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("cons", slip.List{nil, slip.Fixnum(1)}),
		String:    "(cons 1 nil)",
		Simple:    []interface{}{"cons", 1, nil},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: slip.List{slip.Fixnum(1)},
	}).Test(t)
}

func TestConsList(t *testing.T) {
	(&sliptest.Function{
		Source: `(cons 1 '(2))`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestConsArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(cons 1)`,
		Panics: true,
	}).Test(t)
}

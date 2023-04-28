// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCaaddrList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaddr '(a b (c d)))",
		Expect: "c",
	}).Test(t)
}

func TestCaaddrCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaddr '(a . (b . ((c . d)))))",
		Expect: "c",
	}).Test(t)
}

func TestCaaddrSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a . (b . ((c . d)))))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caaddr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a . (b . ((x . d))))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

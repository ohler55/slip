// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCadadrList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadadr '(a (b c d)))",
		Expect: "c",
	}).Test(t)
}

func TestCadadrCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadadr '(a . ((b . (c . d)))))",
		Expect: "c",
	}).Test(t)
}

func TestCadadrSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a  (b c d)))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cadadr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a (b x d))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCddrEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddr nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCddrFoundCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddr '(a . (b . c)))",
		Expect: "c",
	}).Test(t)
}

func TestCddrFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddr '(a b c d))",
		Expect: "(c d)",
	}).Test(t)
}

func TestCddrSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a . (b . c)))").Eval(slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cddr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a . (b . x))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCddrSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a  b  c))").Eval(slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cddr target) 'x)",
		Panics: true,
	}).Test(t)
}

func TestCddrWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddr)",
		Panics: true,
	}).Test(t)
}

func TestCddrWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddr t)",
		Panics: true,
	}).Test(t)
}

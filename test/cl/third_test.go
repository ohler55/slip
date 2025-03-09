// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestThirdEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(third nil)",
		Expect: "nil",
	}).Test(t)
}

func TestThirdList(t *testing.T) {
	(&sliptest.Function{
		Source: "(third '(a b c))",
		Expect: "c",
	}).Test(t)
}

func TestThirdCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(third '(a b . c))",
		Expect: "c",
	}).Test(t)
}

func TestThirdNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(third 7)",
		Panics: true,
	}).Test(t)
}

func TestThirdSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (third target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestThirdSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b . c))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (third target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b . x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestThirdSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (third 7) 'x)",
		Panics: true,
	}).Test(t)
}

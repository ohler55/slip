// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEvalList(t *testing.T) {
	(&sliptest.Function{
		Source: `(eval (+ 1 2))`,
		Expect: "3",
	}).Test(t)
}

func TestEvalListQuoted(t *testing.T) {
	(&sliptest.Function{
		Source: `(eval '(+ 1 2))`,
		Expect: "3",
	}).Test(t)
}

func TestEvalSymbol(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq x '(+ 1 2))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(eval x)`,
		Expect: "3",
	}).Test(t)
}

func TestEvalQuoted(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq x '(+ 1 2))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(eval 'x)`,
		Expect: "(+ 1 2)",
	}).Test(t)
}

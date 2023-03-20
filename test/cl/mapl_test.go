// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMaplLambda(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq collect '())`).Eval(scope)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(mapl (lambda (x y) (setq collect (cons (+ (car x) (car y)) collect))) '(1 2 3 4) '(2 4 6))`,
		Expect: "(1 2 3 4)",
	}).Test(t)
	tt.Equal(t, "(9 6 3)", slip.ObjectString(scope.Get(slip.Symbol("collect"))))
}

func TestMaplNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapl 'print t)`,
		Panics: true,
	}).Test(t)
}

func TestMaplNotList2(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapl 'print '(1 2) t)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMapcLambda(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq collect '())`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(mapc (lambda (x) (setq collect (cons x collect))) '(1 2 3))`,
		Expect: "(1 2 3)",
	}).Test(t)
	tt.Equal(t, "(3 2 1)", slip.ObjectString(scope.Get(slip.Symbol("collect"))))
}

func TestMapcFunction(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq collect '())`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(mapc (lambda (x y) (setq collect (cons (cons x y) collect))) '(a b c d) '(1 2 3))`,
		Expect: "(a b c d)",
	}).Test(t)
	tt.Equal(t, "((c . 3) (b . 2) (a . 1))", slip.ObjectString(scope.Get(slip.Symbol("collect"))))
}

func TestMapcNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapc 'print t)`,
		Panics: true,
	}).Test(t)
}

func TestMapcNotList2(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapc 'print '(1 2) t)`,
		Panics: true,
	}).Test(t)
}

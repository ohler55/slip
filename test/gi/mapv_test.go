// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMapvList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq collect '())`).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(mapv (lambda (x) (setq collect (cons x collect))) '(1 2 3))`,
		Expect: "(1 2 3)",
	}).Test(t)
	tt.Equal(t, "(3 2 1)", slip.ObjectString(scope.Get(slip.Symbol("collect"))))
}

func TestMapvVector(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq collect '())`).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(mapv (lambda (x) (setq collect (cons x collect))) #(1 2 3))`,
		Array:  true,
		Expect: "#(1 2 3)",
	}).Test(t)
	tt.Equal(t, "(3 2 1)", slip.ObjectString(scope.Get(slip.Symbol("collect"))))
}

func TestMapvOctets(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq collect '())`).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(mapv (lambda (x) (setq collect (cons x collect))) (coerce #(1 2 3) 'octets))`,
		Array:  true,
		Expect: "#(1 2 3)",
	}).Test(t)
	tt.Equal(t, "(3 2 1)", slip.ObjectString(scope.Get(slip.Symbol("collect"))))
}

func TestMapvFunction(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq collect '())`).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(mapv (lambda (x y) (setq collect (cons (cons x y) collect))) '(a b c d) '(1 2 3))`,
		Expect: "(a b c d)",
	}).Test(t)
	tt.Equal(t, "((c . 3) (b . 2) (a . 1))", slip.ObjectString(scope.Get(slip.Symbol("collect"))))
}

func TestMapvNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapv 'print t)`,
		Panics: true,
	}).Test(t)
}

func TestMapvNotList2(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapv 'print '(1 2) t)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/sliptest"
)

func TestBagWalkPathLisp(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq result '())`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "[1 2 3]"))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-walk bag (lambda (x) (setq result (cons x result))) "*")`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "(3 2 1)", scope.Get(slip.Symbol("result")).String())
}

func TestBagWalkPathBag(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq result '())`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "[1 2 3]"))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-walk bag '(lambda (x) (setq result (cons (bag-native x) result))) "*" t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "(3 2 1)", scope.Get(slip.Symbol("result")).String())
}

func TestBagWalkFuncLisp(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq result '())`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(defun walk-add (x) (setq result (cons x result)))`, scope).Eval(scope, nil)
	// TBD setup a defer to undefine walk-add
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "[1 2 3]"))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-walk bag 'walk-add "*")`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "(3 2 1)", scope.Get(slip.Symbol("result")).String())
}

func TestBagWalkFuncBag(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq result '())`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(defun walk-add-bag (x) (setq result (cons (bag-native x) result)))`, scope).Eval(scope, nil)
	// TBD setup a defer to undefine walk-add
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "[1 2 3]"))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-walk bag #'walk-add-bag (make-bag-path "*") t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "(3 2 1)", scope.Get(slip.Symbol("result")).String())
}

func TestBagWalkArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-walk (make-instance 'bag-flavor))`,
		Panics: true,
	}).Test(t)
}

func TestBagWalkNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-walk (make-instance 'vanilla-flavor) (lambda (x) nil))`,
		Panics: true,
	}).Test(t)
}

func TestBagWalkNotPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-walk (make-instance 'bag-flavor) (lambda (x) nil) t)`,
		Panics: true,
	}).Test(t)
}

func TestBagWalkNotFunction(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-walk (make-instance 'bag-flavor) t)`,
		Panics: true,
	}).Test(t)
}

func TestBagWalkProcedure(t *testing.T) {
	bag.SetCompileScript(nil)
	(&sliptest.Function{
		Source: `(let (result)
                  (bag-walk (make-bag "[1 2 3]") (lambda (v) (addf result v)) "[((lambda (x) (mapcar '1+ x)))]")
                  result)`,
		Expect: "(2 3 4)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let (result)
                  (bag-walk (make-bag "[1 2 3]") (lambda (v) (addf result v)) "[(+)]")
                  result)`,
		Expect: "(6)",
	}).Test(t)
}

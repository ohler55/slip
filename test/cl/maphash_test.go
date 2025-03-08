// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMaphashLambda(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(setq table (make-hash-table))
(setf (gethash 'a table) 1)
(setq entries '())
`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(maphash (lambda (k v) (setq entries (cons (list k v) entries))) table)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, slip.List{slip.List{slip.Symbol("a"), slip.Fixnum(1)}}, scope.Get(slip.Symbol("entries")))
}

func TestMaphashLambdaList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(setq table (make-hash-table))
(setf (gethash 'a table) 1)
(setq entries '())
`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(maphash '(lambda (k v) (setq entries (cons (list k v) entries))) table)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, slip.List{slip.List{slip.Symbol("a"), slip.Fixnum(1)}}, scope.Get(slip.Symbol("entries")))
}

func TestMaphashSymbol(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(setq table (make-hash-table))
(setf (gethash 'a table) 1)
(setq entries '())
(defun entry-push (k v) (setq entries (cons (list k v) entries)))
`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(maphash 'entry-push table)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, slip.List{slip.List{slip.Symbol("a"), slip.Fixnum(1)}}, scope.Get(slip.Symbol("entries")))
}

func TestMaphashArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(maphash 'entry-push)`,
		Panics: true,
	}).Test(t)
}

func TestMaphashNotHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(maphash 'entry-push t)`,
		Panics: true,
	}).Test(t)
}

func TestMaphashNotFunction(t *testing.T) {
	(&sliptest.Function{
		Source: `(maphash t (make-hash-table))`,
		Panics: true,
	}).Test(t)
}

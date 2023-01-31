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
`).Eval(scope)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(maphash (lambda (k v) (setq entries (cons (list k v) entries))) table)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, slip.List{slip.List{slip.Fixnum(1), slip.Symbol("a")}}, scope.Get(slip.Symbol("entries")))
}

func xTestMaphashSymbol(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(setq table (make-hash-table))
(setf (gethash 'a table) 1)
(setq entries '())
(defun entry-push (k v) (setq entries (cons (list k v) entries)))
`).Eval(scope)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(maphash 'entry-push table)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, slip.List{slip.List{slip.Fixnum(1), slip.Symbol("a")}}, scope.Get(slip.Symbol("entries")))
}

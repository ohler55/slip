// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGethashOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq table (make-hash-table))").Eval(scope)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(gethash 'a table)`,
		Expect: "nil, nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setf (gethash 'a table) 1)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(gethash 'a table)`,
		Expect: "1, t",
	}).Test(t)
}

func TestGethashArgCount(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq table (make-hash-table))").Eval(scope)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(gethash 'a table t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setf (gethash 'a table t) 1)`,
		Panics: true,
	}).Test(t)
}

func TestGethashNotHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(gethash 'a t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(setf (gethash 'a t) 1)`,
		Panics: true,
	}).Test(t)
}

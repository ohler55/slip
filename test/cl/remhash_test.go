// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRemhashOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(setq table (make-hash-table))
(setf (gethash 'a table) 1)
`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(remhash 'a table)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(remhash 'a table)`,
		Expect: "nil",
	}).Test(t)
}

func TestRemhashArgCount(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq table (make-hash-table))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(remhash 'a table t)`,
		Panics: true,
	}).Test(t)
}

func TestRemhashNotHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(remhash 'a t)`,
		Panics: true,
	}).Test(t)
}

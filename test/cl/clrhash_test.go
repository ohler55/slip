// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClrhashOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(setq table (make-hash-table))
(setf (gethash 'a table) 1)
`).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(clrhash table)`,
		Expect: "#<hash-table eql 0/-->",
	}).Test(t)
}

func TestClrhashArgCount(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq table (make-hash-table))").Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(clrhash table t)`,
		Panics: true,
	}).Test(t)
}

func TestClrhashNotHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(clrhash t)`,
		Panics: true,
	}).Test(t)
}

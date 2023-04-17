// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestHashTableCount(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq table (make-hash-table))").Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(hash-table-count table)`,
		Expect: "0",
	}).Test(t)
	_ = slip.ReadString("(setf (gethash 'one table) 1)").Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(hash-table-count table)`,
		Expect: "1",
	}).Test(t)
}

func TestHashTableCountBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-count)`,
		Panics: true,
	}).Test(t)
}

func TestHashTableCountBadHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-count t)`,
		Panics: true,
	}).Test(t)
}

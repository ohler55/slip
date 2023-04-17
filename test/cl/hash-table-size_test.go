// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestHashTableSize(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq table (make-hash-table))").Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(hash-table-size table)`,
		Expect: "0",
	}).Test(t)
	_ = slip.ReadString("(setf (gethash 'one table) 1)").Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(hash-table-size table)`,
		Expect: "1",
	}).Test(t)
}

func TestHashTableSizeBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-size)`,
		Panics: true,
	}).Test(t)
}

func TestHashTableSizeBadHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-size t)`,
		Panics: true,
	}).Test(t)
}

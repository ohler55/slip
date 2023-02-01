// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestHashTableRehashSize(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq table (make-hash-table))").Eval(scope)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(hash-table-rehash-size table)`,
		Expect: "1",
	}).Test(t)
}

func TestHashTableRehashSizeBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-rehash-size)`,
		Panics: true,
	}).Test(t)
}

func TestHashTableRehashSizeBadHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-rehash-size t)`,
		Panics: true,
	}).Test(t)
}

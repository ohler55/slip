// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestHashTableRehashThreshold(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq table (make-hash-table))").Eval(scope)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(hash-table-rehash-threshold table)`,
		Expect: "0",
	}).Test(t)
}

func TestHashTableRehashThresholdBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-rehash-threshold)`,
		Panics: true,
	}).Test(t)
}

func TestHashTableRehashThresholdBadHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-rehash-threshold t)`,
		Panics: true,
	}).Test(t)
}

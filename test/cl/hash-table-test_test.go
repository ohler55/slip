// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestHashTableTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-test (make-hash-table))`,
		Expect: "eql",
	}).Test(t)
}

func TestHashTableArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-test (make-hash-table) t)`,
		Panics: true,
	}).Test(t)
}

func TestHashTableNotHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-test t)`,
		Panics: true,
	}).Test(t)
}

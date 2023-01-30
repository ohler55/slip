// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestHashTablepTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-p (make-hash-table))`,
		Expect: "t",
	}).Test(t)
}

func TestHashTablepFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-p t)`,
		Expect: "nil",
	}).Test(t)
}

func TestHashTablepBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(hash-table-p)`,
		Panics: true,
	}).Test(t)
}

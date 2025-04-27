// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestLdbTestTrue(t *testing.T) {
	(&sliptest.Function{
		Source: "(ldb-test (byte 4 1) 16)",
		Expect: "t",
	}).Test(t)
}

func TestLdbTestFalse(t *testing.T) {
	(&sliptest.Function{
		Source: "(ldb-test (byte 3 1) 16)",
		Expect: "nil",
	}).Test(t)
}

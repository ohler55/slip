// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMultipleValueListValues(t *testing.T) {
	(&sliptest.Function{
		Source: `(multiple-value-list (floor 5 3))`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestMultipleValueListSingle(t *testing.T) {
	(&sliptest.Function{
		Source: `(multiple-value-list 3)`,
		Expect: "(3)",
	}).Test(t)
}

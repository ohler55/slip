// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMultipleValueCallBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(multiple-value-call #'list (floor 5 3) 0 (floor 13 4))`,
		Expect: "(1 2 0 3 1)",
	}).Test(t)
}

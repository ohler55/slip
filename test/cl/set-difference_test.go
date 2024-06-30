// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSetDifference(t *testing.T) {
	(&sliptest.Function{
		Source: `(set-difference '(a b c) '(b d))`,
		Expect: "(a c)",
	}).Test(t)
}

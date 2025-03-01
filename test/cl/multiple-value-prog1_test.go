// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMultipleValueProg1(t *testing.T) {
	(&sliptest.Function{
		Source: `(multiple-value-prog1 (floor 5 3) (+ 2 3))`,
		Expect: "1, 2",
	}).Test(t)
}

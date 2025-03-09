// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestConstantlyFunction(t *testing.T) {
	(&sliptest.Function{
		Source: `(constantly 4)`,
		Expect: `/^#<function \(lambda \(&rest arguments\)\) \{[0-9a-f]+\}>$/`,
	}).Test(t)
}

func TestConstantlyInUse(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcar (constantly 5) '(1 2 3))`,
		Expect: "(5 5 5)",
	}).Test(t)
}

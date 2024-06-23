// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRandomStatepTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(random-state-p (make-random-state))`,
		Expect: "t",
	}).Test(t)
}

func TestRandomStatepFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(random-state-p t)`,
		Expect: "nil",
	}).Test(t)
}

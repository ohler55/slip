// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFunctionpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(functionp #'car)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(functionp (lambda () nil))`,
		Expect: "t",
	}).Test(t)
}

func TestFunctionpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(functionp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestFunctionpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(functionp)`,
		Panics: true,
	}).Test(t)
}

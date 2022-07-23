// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestIntegertpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(integerp 4)`,
		Expect: "t",
	}).Test(t)
}

func TestIntegertpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(integerp 4.2)`,
		Expect: "nil",
	}).Test(t)
}

func TestIntegertpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(integerp)`,
		Panics: true,
	}).Test(t)
}

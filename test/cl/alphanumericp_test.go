// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAlphanumericpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(alphanumericp #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(alphanumericp #\π)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(alphanumericp #\7)`,
		Expect: "t",
	}).Test(t)
}

func TestAlphanumericpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(alphanumericp #\-)`,
		Expect: "nil",
	}).Test(t)
}

func TestAlphanumericpNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(alphanumericp 7)`,
		Panics: true,
	}).Test(t)
}

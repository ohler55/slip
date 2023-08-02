// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(char "abc" 1)`,
		Expect: `#\b`,
	}).Test(t)
}

func TestCharOutOfBounds(t *testing.T) {
	(&sliptest.Function{
		Source: `(char "abc" 3)`,
		Panics: true,
	}).Test(t)
}

func TestCharNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(char 7 1)`,
		Panics: true,
	}).Test(t)
}

func TestCharNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source: `(char "abc" t)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestScharOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(schar "abc" 1)`,
		Expect: `#\b`,
	}).Test(t)
}

func TestScharOutOfBounds(t *testing.T) {
	(&sliptest.Function{
		Source: `(schar "abc" 3)`,
		Panics: true,
	}).Test(t)
}

func TestScharNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(schar 7 1)`,
		Panics: true,
	}).Test(t)
}

func TestScharNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source: `(schar "abc" t)`,
		Panics: true,
	}).Test(t)
}

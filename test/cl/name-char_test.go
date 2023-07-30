// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNameCharAlpha(t *testing.T) {
	(&sliptest.Function{
		Source: `(name-char "Capital-A")`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(name-char "small-a")`,
		Expect: `#\a`,
	}).Test(t)
}

func TestNameCharNonGraphic(t *testing.T) {
	(&sliptest.Function{
		Source: `(name-char 'tab)`,
		Expect: `#\Tab`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(name-char 'nak)`,
		Expect: `#\u0015`,
	}).Test(t)
}

func TestNameCharHiBit(t *testing.T) {
	(&sliptest.Function{
		Source: `(name-char "#\\π")`,
		Expect: `#\π`,
	}).Test(t)
}

func TestNameCharNoMatch(t *testing.T) {
	(&sliptest.Function{
		Source: `(name-char "not-a-good-name")`,
		Expect: `nil`,
	}).Test(t)
}

func TestNameCharNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(name-char 7)`,
		Panics: true,
	}).Test(t)
}

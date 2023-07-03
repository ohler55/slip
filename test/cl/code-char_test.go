// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCodeCharOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(code-char 65)`,
		Expect: `#\A`,
	}).Test(t)
}

func TestCodeCharNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(code-char -7)`,
		Expect: "nil",
	}).Test(t)
}

func TestCodeCharNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(code-char 'x)`,
		Panics: true,
	}).Test(t)
}

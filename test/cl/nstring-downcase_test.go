// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNstringDowncaseBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(nstring-downcase "aBc")`,
		Expect: `"abc"`,
	}).Test(t)
}

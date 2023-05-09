// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNstringUpcaseBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(nstring-upcase "aBc")`,
		Expect: `"ABC"`,
	}).Test(t)
}

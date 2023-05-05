// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNstringCapitalizeBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(nstring-capitalize "aBc DEF")`,
		Expect: `"Abc Def"`,
	}).Test(t)
}

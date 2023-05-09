// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringCapitalizeBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-capitalize "aBc DEF")`,
		Expect: `"Abc Def"`,
	}).Test(t)
}

func TestStringCapitalizeStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-capitalize "aBcDeF" :start 2 :end 4)`,
		Expect: `"aBCdeF"`,
	}).Test(t)
}

func TestStringCapitalizeNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-capitalize 123)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringUpcaseBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-upcase "aBc")`,
		Expect: `"ABC"`,
	}).Test(t)
}

func TestStringUpcaseStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-upcase "aBcDeF" :start 2 :end 4)`,
		Expect: `"aBCDeF"`,
	}).Test(t)
}

func TestStringUpcaseNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-upcase 123)`,
		Panics: true,
	}).Test(t)
}

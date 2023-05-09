// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string "abc")`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestStringSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(string 'abc)`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestStringCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(string #\A)`,
		Expect: `"A"`,
	}).Test(t)
}

func TestStringOther(t *testing.T) {
	(&sliptest.Function{
		Source: `(string 123)`,
		Expect: `"123"`,
	}).Test(t)
}

func TestStringBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(string)`,
		Panics: true,
	}).Test(t)
}

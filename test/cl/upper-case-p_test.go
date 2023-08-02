// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestUpperCasePTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(upper-case-p #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(upper-case-p #\Π)`,
		Expect: "t",
	}).Test(t)
}

func TestUpperCasePFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(upper-case-p #\a)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(upper-case-p #\-)`,
		Expect: "nil",
	}).Test(t)
}

func TestUpperCasePNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(upper-case-p 7)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestLowerCasePTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(lower-case-p #\a)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(lower-case-p #\π)`,
		Expect: "t",
	}).Test(t)
}

func TestLowerCasePFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(lower-case-p #\A)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(lower-case-p #\-)`,
		Expect: "nil",
	}).Test(t)
}

func TestLowerCasePNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(lower-case-p 7)`,
		Panics: true,
	}).Test(t)
}

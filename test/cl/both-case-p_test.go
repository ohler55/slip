// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBothCasePTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(both-case-p #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(both-case-p #\π)`,
		Expect: "t",
	}).Test(t)
}

func TestBothCasePFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(both-case-p #\-)`,
		Expect: "nil",
	}).Test(t)
}

func TestBothCasePNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(both-case-p 7)`,
		Panics: true,
	}).Test(t)
}

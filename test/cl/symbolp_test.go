// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSymboltpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbolp 'abc)`,
		Expect: "t",
	}).Test(t)
}

func TestSymboltpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbolp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestSymboltpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbolp)`,
		Panics: true,
	}).Test(t)
}

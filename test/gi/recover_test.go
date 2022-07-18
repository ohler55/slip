// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRecoverCatch(t *testing.T) {
	(&sliptest.Function{
		Source: `(recover rec rec 'abc (panic 'catch-me) 'def)`,
		Expect: "catch-me",
	}).Test(t)
}

func TestRecoverOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(recover rec rec 'abc 'def)`,
		Expect: "def",
	}).Test(t)
}

func TestRecoverArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(recover x)`,
		Panics: true,
	}).Test(t)
}

func TestRecoverSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(recover t 7)`,
		Panics: true,
	}).Test(t)
}

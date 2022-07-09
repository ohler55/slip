// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPanic(t *testing.T) {
	(&sliptest.Function{
		Source: `(panic "catch-me")`,
		Panics: true,
	}).Test(t)
}

func TestPanicArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(panic)`,
		Panics: true,
	}).Test(t)
}

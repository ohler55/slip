// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharacterpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(characterp #\A)`,
		Expect: "t",
	}).Test(t)
}

func TestCharacterpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(characterp 7)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharacterpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(characterp)`,
		Panics: true,
	}).Test(t)
}

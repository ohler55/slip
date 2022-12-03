// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStreampTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(streamp *standard-output*)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(streamp *standard-input*)`,
		Expect: "t",
	}).Test(t)
}

func TestStreampFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(streamp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestStreampBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(streamp)`,
		Panics: true,
	}).Test(t)
}

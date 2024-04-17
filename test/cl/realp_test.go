// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRealpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(realp 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(realp 4.1)`,
		Expect: "t",
	}).Test(t)
}

func TestRealpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(realp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestRealpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(realp)`,
		Panics: true,
	}).Test(t)
}

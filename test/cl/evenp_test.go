// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestEvenpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 4)`,
		Expect: "t",
	}).Test(t)
}

func TestEvenpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 5)`,
		Expect: "nil",
	}).Test(t)
}

func TestEvenpBigTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 12297829382473034410)`,
		Expect: "t",
	}).Test(t)
}

func TestEvenpBigFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 12297829382473034411)`,
		Expect: "nil",
	}).Test(t)
}

func TestEvenpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp)`,
		Panics: true,
	}).Test(t)
}

func TestEvenpNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 7.0)`,
		Panics: true,
	}).Test(t)
}

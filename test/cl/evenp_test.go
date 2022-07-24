// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestEventpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 4)`,
		Expect: "t",
	}).Test(t)
}

func TestEventpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 5)`,
		Expect: "nil",
	}).Test(t)
}

func TestEventpBigTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 12297829382473034410)`,
		Expect: "t",
	}).Test(t)
}

func TestEventpBigFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 12297829382473034411)`,
		Expect: "nil",
	}).Test(t)
}

func TestEventpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp)`,
		Panics: true,
	}).Test(t)
}

func TestEventpNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source: `(evenp 7.0)`,
		Panics: true,
	}).Test(t)
}

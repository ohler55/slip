// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLognotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(lognot 7)`,
		Expect: "-8",
	}).Test(t)
}

func TestLognotBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (lognot #x333333333333333333))`,
		Expect: `"cccccccccccccccccc"`, // same as -333333333333333334
	}).Test(t)
}

func TestLognotNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(lognot t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

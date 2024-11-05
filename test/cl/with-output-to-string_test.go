// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWithOutputToString(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (s) (princ 'abc s))`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestWithOutputToStringBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-output-to-string t (princ 'x))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithOutputToStringNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-output-to-string (t) (princ 'x))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

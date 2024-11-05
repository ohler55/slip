// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWithOpenStreamBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-open-stream (s (make-string-input-stream "abc def")) (read s))`,
		Expect: "abc",
	}).Test(t)
}

func TestWithOpenStreamBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-open-stream t (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithOpenStreamBadVar(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-open-stream (t "abc") (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithOpenStreamNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-open-stream (x t) (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

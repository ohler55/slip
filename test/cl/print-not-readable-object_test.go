// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPrintNotReadableExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(print-not-readable-object (make-condition 'print-not-readable :object 'something))`,
		Expect: "something",
	}).Test(t)
}

func TestPrintNotReadableWrongCondition(t *testing.T) {
	(&sliptest.Function{
		Source:    `(print-not-readable-object (make-condition 'error :object '(1 2)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

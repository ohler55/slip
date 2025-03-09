// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestShiftfOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x '(1 2 3 4 5)))
                  (shiftf (nth 1 x) (nth 2 x) (nth 3 x))
                  x)`,
		Expect: "(1 4 2 3 5)",
	}).Test(t)
}

func TestShiftfNotPlace(t *testing.T) {
	(&sliptest.Function{
		Source:    `(shiftf (+ 1 2))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

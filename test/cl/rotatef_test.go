// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRotatefOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x '(1 2 3 4 5)))
                  (rotatef (nth 1 x) (nth 2 x) (nth 3 x))
                  x)`,
		Expect: "(1 3 4 2 5)",
	}).Test(t)
}

func TestRotatefNotPlace(t *testing.T) {
	(&sliptest.Function{
		Source:    `(rotatef (+ 1 2))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

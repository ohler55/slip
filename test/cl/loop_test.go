// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLoopSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0))
                  (loop
                   (setq x (1+ x))
                   (when (< 3 x) (return x))))`,
		Expect: "4",
	}).Test(t)
}

func TestLoopBlock(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0))
                  (block square
                   (loop
                    (setq x (1+ x))
                    (when (< 3 x) (return-from square x)))))`,
		Expect: "4",
	}).Test(t)
}

func TestLoopReturnFrom(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0))
                  (loop
                   (setq x (1+ x))
                   (when (< 3 x) (return-from square x))))`,
		PanicType: slip.ControlErrorSymbol,
	}).Test(t)
}

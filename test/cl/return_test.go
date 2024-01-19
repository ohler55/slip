// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReturnOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (+ 1 2) (return 1) (+ 2 3))`,
		Expect: "1",
	}).Test(t)
}

func TestReturnEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (return) (+ 2 3))`,
		Expect: "nil",
	}).Test(t)
}

func TestReturnNested(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (let () (block nil (let () (return 1)) 2) 3))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(block xyz (let () (block nil (let () (return 1)) 2) 3))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(block nil (let () (block xyz (let () (return 1)) 2) 3))`,
		Expect: "1",
	}).Test(t)
}

func TestReturnNotInBlock(t *testing.T) {
	(&sliptest.Function{
		Source:    `(return 1)`,
		PanicType: slip.Symbol("control-error"),
	}).Test(t)
}

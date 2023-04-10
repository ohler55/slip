// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPrognEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn)`,
		Expect: "nil",
	}).Test(t)
}

func TestPrognBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn 1 2 (+ 1 2))`,
		Expect: "3",
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestProg1Basic(t *testing.T) {
	(&sliptest.Function{
		Source: `(prog1 1 2 (+ 1 2))`,
		Expect: "1",
	}).Test(t)
}

func TestProg1Empty(t *testing.T) {
	(&sliptest.Function{
		Source: `(prog1)`,
		Panics: true,
	}).Test(t)
}

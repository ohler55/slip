// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNconcEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc)`,
		Expect: "nil",
	}).Test(t)
}

func TestNconcListList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc '(a b) '(c d) '(e f))`,
		Expect: "(a b c d e f)",
	}).Test(t)
}

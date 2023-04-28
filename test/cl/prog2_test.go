// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestProg2Basic(t *testing.T) {
	(&sliptest.Function{
		Source: `(prog2 1 2 (+ 1 2))`,
		Expect: "2",
	}).Test(t)
}

func TestProg2Empty(t *testing.T) {
	(&sliptest.Function{
		Source: `(prog2)`,
		Panics: true,
	}).Test(t)
}

func TestProg2One(t *testing.T) {
	(&sliptest.Function{
		Source: `(prog2 1)`,
		Panics: true,
	}).Test(t)
}

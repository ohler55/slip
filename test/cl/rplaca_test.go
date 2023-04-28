// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRplacaList(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplaca '(a b) 'x)`,
		Expect: "(x b)",
	}).Test(t)
}

func TestRplacaCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplaca '(a . b) 'x)`,
		Expect: "(x . b)",
	}).Test(t)
}

func TestRplacaBadCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplaca t 'x)`,
		Panics: true,
	}).Test(t)
}

func TestRplacaArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplaca '(a))`,
		Panics: true,
	}).Test(t)
}

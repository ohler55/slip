// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRplacdList(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplacd '(a b) 'x)`,
		Expect: "(a . x)",
	}).Test(t)
}

func TestRplacdCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplacd '(a . b) 'x)`,
		Expect: "(a . x)",
	}).Test(t)
}

func TestRplacdShortList(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplacd '(a) 'x)`,
		Expect: "(a . x)",
	}).Test(t)
}

func TestRplacdListList(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplacd '(a) '(b c))`,
		Expect: "(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rplacd '(a b) '(c d))`,
		Expect: "(a c d)",
	}).Test(t)
}

func TestRplacdBadCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplacd t 'x)`,
		Panics: true,
	}).Test(t)
}

func TestRplacdArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(rplacd '(a))`,
		Panics: true,
	}).Test(t)
}

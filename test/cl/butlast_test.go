// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestButlastNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(butlast nil)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(butlast '())`,
		Expect: "nil",
	}).Test(t)
}

func TestButlastOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(butlast '(a b c))`,
		Expect: "(a b)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(butlast '(a b . c))`,
		Expect: "(a)",
	}).Test(t)
}

func TestButlastN(t *testing.T) {
	(&sliptest.Function{
		Source: `(butlast '(a b c d) 2)`,
		Expect: "(a b)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(butlast '(a b c . d) 2)`,
		Expect: "(a)",
	}).Test(t)
}

func TestButlastBadN(t *testing.T) {
	(&sliptest.Function{
		Source: `(butlast '(a b c d) t)`,
		Panics: true,
	}).Test(t)
}

func TestButlastBadList(t *testing.T) {
	(&sliptest.Function{
		Source: `(butlast t)`,
		Panics: true,
	}).Test(t)
}

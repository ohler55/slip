// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNbutlastNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(nbutlast nil)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nbutlast '())`,
		Expect: "nil",
	}).Test(t)
}

func TestNbutlastOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(nbutlast '(a b c))`,
		Expect: "(a b)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nbutlast '(a b . c))`,
		Expect: "(a)",
	}).Test(t)
}

func TestNbutlastN(t *testing.T) {
	(&sliptest.Function{
		Source: `(nbutlast '(a b c d) 2)`,
		Expect: "(a b)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nbutlast '(a b c . d) 2)`,
		Expect: "(a)",
	}).Test(t)
}

func TestNbutlastBadN(t *testing.T) {
	(&sliptest.Function{
		Source: `(nbutlast '(a b c d) t)`,
		Panics: true,
	}).Test(t)
}

func TestNbutlastBadList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nbutlast t)`,
		Panics: true,
	}).Test(t)
}

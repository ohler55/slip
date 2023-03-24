// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestLastNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(last nil)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(last '())`,
		Expect: "nil",
	}).Test(t)
}

func TestLastOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(last '(a b c))`,
		Expect: "(c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(last '(a b . c))`,
		Expect: "(b . c)",
	}).Test(t)
}

func TestLastN(t *testing.T) {
	(&sliptest.Function{
		Source: `(last '(a b c d) 2)`,
		Expect: "(c d)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(last '(a b c . d) 2)`,
		Expect: "(b c . d)",
	}).Test(t)
}

func TestLastConsZero(t *testing.T) {
	(&sliptest.Function{
		Source: `(last '(a . b) 0)`,
		Expect: "b",
	}).Test(t)
}

func TestLastCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(last '(a . b) 1)`,
		Expect: "(a . b)",
	}).Test(t)
}

func TestLastBadN(t *testing.T) {
	(&sliptest.Function{
		Source: `(last '(a b c d) t)`,
		Panics: true,
	}).Test(t)
}

func TestLastBadList(t *testing.T) {
	(&sliptest.Function{
		Source: `(last t)`,
		Panics: true,
	}).Test(t)
}

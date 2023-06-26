// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCoerceTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 3 t)`,
		Expect: "3",
	}).Test(t)
}

func TestCoerceToList(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce nil 'list)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(a b c) 'list)`,
		Expect: "(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #(a b c) 'list)`,
		Expect: "(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce "abc" 'list)`,
		Expect: `(#\a #\b #\c)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 'abc 'list)`,
		Expect: `(#\a #\b #\c)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3 'list)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce nil 'vector)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(a b c) 'vector)`,
		Expect: "#(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #(a b c) 'vector)`,
		Expect: "#(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce "abc" 'vector)`,
		Expect: `#(#\a #\b #\c)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 'abc 'vector)`,
		Expect: `#(#\a #\b #\c)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3 'vector)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToString(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce nil 'string)`,
		Expect: `""`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 'abc 'string)`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce "abc" 'string)`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(#\a #\b #\c) 'string)`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #(#\a #\b #\c) 'string)`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3 'string)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(#\a b) 'string)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceBadType(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 3 5)`,
		Panics: true,
	}).Test(t)
}

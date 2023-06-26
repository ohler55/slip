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

func TestCoerceToCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce #\A 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 65 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce -1 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 65.0 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 65.3 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 130/2 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 130/3 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(65 0) 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(65 1) 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce nil 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(a) 'character)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToInteger(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 123 'integer)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'integer)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0 'integer)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1 'integer)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0L+0 'integer)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1L+0 'integer)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 10/2 'integer)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 11/2 'integer)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'integer)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'integer)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (- 30000000000000000123 30000000000000000000) 'integer)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'integer)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 123 'fixnum)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'fixnum)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0 'fixnum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0L+0 'fixnum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1L+0 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 10/2 'fixnum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 11/2 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'fixnum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (- 30000000000000000123 30000000000000000000) 'fixnum)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 30000000000000000123 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'fixnum)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceBadType(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 3 5)`,
		Panics: true,
	}).Test(t)
}

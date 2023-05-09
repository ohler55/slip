// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestConcatenateList(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'list "abc" '(d e f) #(1 2 3))`,
		Expect: `(#\a #\b #\c d e f 1 2 3)`,
	}).Test(t)
}

func TestConcatenateString(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'string "abc" '(#\d #\e #\f) #(#\g #\h #\i))`,
		Expect: `"abcdefghi"`,
	}).Test(t)
}

func TestConcatenateVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'vector "abc" '(d e f) #(1 2 3))`,
		Expect: `#(#\a #\b #\c d e f 1 2 3)`,
	}).Test(t)
}

func TestConcatenateBadResultType(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'fixnum "abc" '(d e f) #(1 2 3))`,
		Panics: true,
	}).Test(t)
}

func TestConcatenateBadList(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'list t)`,
		Panics: true,
	}).Test(t)
}

func TestConcatenateBadString(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'string t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(concatenate 'string '(#\a t))`,
		Panics: true,
	}).Test(t)
}

func TestConcatenateBadVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'vector t)`,
		Panics: true,
	}).Test(t)
}

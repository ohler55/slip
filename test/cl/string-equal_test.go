// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringEqualTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal "aBc" "AbC")`,
		Expect: "t",
	}).Test(t)
}

func TestStringEqualFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal "abc" "abcd")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringEqualCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal #\A #\a)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-equal "a" #\a)`,
		Expect: "t",
	}).Test(t)
}

func TestStringEqualSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal 'ABC 'abc)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-equal "abc" 'abc)`,
		Expect: "t",
	}).Test(t)
}

func TestStringEqualStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal "xbcdef" "xabcdex" :start1 1 :end1 4 :start2 2 :end2 5)`,
		Expect: "t",
	}).Test(t)
}

func TestStringEqualNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal t "x")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-equal "x" t)`,
		Panics: true,
	}).Test(t)
}

func TestStringEqualNotKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal "x" "x" t 1)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-equal "x" "x" :bad 1)`,
		Panics: true,
	}).Test(t)
}

func TestStringEqualMissingKeywordValue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal "x" "x" :start1)`,
		Panics: true,
	}).Test(t)
}

func TestStringEqualBadStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal "x" "x" :start1 t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-equal "x" "x" :end1 t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-equal "x" "x" :start2 t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-equal "x" "x" :end2 t)`,
		Panics: true,
	}).Test(t)
}

func TestStringEqualStartEndRange(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-equal "abc" "abc" :start1 3 :end1 2)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-equal "abc" "abc" :start2 3 :end2 2)`,
		Panics: true,
	}).Test(t)
}

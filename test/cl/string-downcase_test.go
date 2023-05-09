// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringDowncaseBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-downcase "aBc")`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestStringDowncaseStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-downcase "aBcDeF" :start 2 :end 4)`,
		Expect: `"aBcdeF"`,
	}).Test(t)
}

func TestStringDowncaseArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-downcase)`,
		Panics: true,
	}).Test(t)
}

func TestStringDowncaseNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-downcase 123)`,
		Panics: true,
	}).Test(t)
}

func TestStringDowncaseStartNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-downcase "abcd" :start 2.5)`,
		Panics: true,
	}).Test(t)
}

func TestStringDowncaseEndNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-downcase "abcd" :end 2.5)`,
		Panics: true,
	}).Test(t)
}

func TestStringDowncaseEndLessThanStart(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-downcase "abcd" :end 1 :start 2)`,
		Panics: true,
	}).Test(t)
}

func TestStringDowncaseBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-downcase "abcd" :bad 2)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-downcase "abcd" t 2)`,
		Panics: true,
	}).Test(t)
}

func TestStringDowncaseMissingKeywordArg(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-downcase "abcd" :start)`,
		Panics: true,
	}).Test(t)
}

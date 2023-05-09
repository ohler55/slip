// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringTrimString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-trim " " "  abc ")`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestStringTrimList(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-trim '(#\Space) "  abc ")`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestStringTrimBadCutset(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-trim t "  abc ")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-trim '(t) "  abc ")`,
		Panics: true,
	}).Test(t)
}

func TestStringTrimNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-trim '(#\Space) t)`,
		Panics: true,
	}).Test(t)
}

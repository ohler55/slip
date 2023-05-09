// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringRightTrimString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-right-trim " " "  abc ")`,
		Expect: `"  abc"`,
	}).Test(t)
}

func TestStringRightTrimList(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-right-trim '(#\Space) "  abc ")`,
		Expect: `"  abc"`,
	}).Test(t)
}

func TestStringRightTrimBadCutset(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-right-trim t "  abc ")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-right-trim '(t) "  abc ")`,
		Panics: true,
	}).Test(t)
}

func TestStringRightTrimNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-right-trim '(#\Space) t)`,
		Panics: true,
	}).Test(t)
}

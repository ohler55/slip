// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringLeftTrimString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-left-trim " " "  abc ")`,
		Expect: `"abc "`,
	}).Test(t)
}

func TestStringLeftTrimList(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-left-trim '(#\Space) "  abc ")`,
		Expect: `"abc "`,
	}).Test(t)
}

func TestStringLeftTrimBadCutset(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-left-trim t "  abc ")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-left-trim '(t) "  abc ")`,
		Panics: true,
	}).Test(t)
}

func TestStringLeftTrimNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-left-trim '(#\Space) t)`,
		Panics: true,
	}).Test(t)
}

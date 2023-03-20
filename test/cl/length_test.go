// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestLengthString(t *testing.T) {
	(&sliptest.Function{
		Source: `(length "abc")`,
		Expect: "3",
	}).Test(t)
}

func TestLengthList(t *testing.T) {
	(&sliptest.Function{
		Source: `(length '(a b c))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(length nil)`,
		Expect: "0",
	}).Test(t)
}

func TestLengthFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(length t)`,
		Panics: true,
	}).Test(t)
}

func TestLengthBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(length)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestListtpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(listp '())`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(listp '(1 2))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(listp '(1 . 2))`,
		Expect: "t",
	}).Test(t)
}

func TestListtpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(listp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestListtpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(listp)`,
		Panics: true,
	}).Test(t)
}

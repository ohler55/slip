// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSxhashOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(sxhash 'abc)`,
		Expect: "198",
	}).Test(t)
	(&sliptest.Function{
		Source: `(sxhash 'aBc)`,
		Expect: "198",
	}).Test(t)
}

func TestSxhashArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(sxhash)`,
		Panics: true,
	}).Test(t)
}

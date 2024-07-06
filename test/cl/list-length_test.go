// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestListLengthOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(list-length '(a b c))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(list-length nil)`,
		Expect: "0",
	}).Test(t)
}

func TestListLengthNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(list-length t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

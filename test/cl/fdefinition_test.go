// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFdefinitionOkay(t *testing.T) {
	(&sliptest.Function{
		Source: `(fdefinition 'car)`,
		Expect: "#<built-in car>",
	}).Test(t)
}

func TestFdefinitionNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fdefinition 'not-a-function)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestFdefinitionNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fdefinition 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSpecialOperatorPTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(special-operator-p 'let)`,
		Expect: "t",
	}).Test(t)
}

func TestSpecialOperatorPFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(special-operator-p 'car)`,
		Expect: "nil",
	}).Test(t)
}

func TestSpecialOperatorPNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(special-operator-p t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMethodCombinationErrorBasic(t *testing.T) {
	(&sliptest.Function{
		Source:    `(method-combination-error :mess "test")`,
		PanicType: slip.Symbol("invalid-method-error"),
	}).Test(t)
}

func TestMethodCombinationErrorNotMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(method-combination-error 7 "test")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestMethodCombinationErrorNotControl(t *testing.T) {
	(&sliptest.Function{
		Source:    `(method-combination-error :mess 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

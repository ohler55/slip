// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestInvalidMethodErrorBasic(t *testing.T) {
	(&sliptest.Function{
		Source:    `(invalid-method-error :mess "test")`,
		PanicType: slip.Symbol("invalid-method-error"),
	}).Test(t)
}

func TestInvalidMethodErrorNotMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(invalid-method-error 7 "test")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestInvalidMethodErrorNotControl(t *testing.T) {
	(&sliptest.Function{
		Source:    `(invalid-method-error :mess 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArithmeticErrorOperationExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(arithmetic-error-operation (make-condition 'arithmetic-error :operation 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestArithmeticErrorOperationNotArithmeticError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(arithmetic-error-operation (make-condition 'error :operation 'test))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

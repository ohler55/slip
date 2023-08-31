// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArithmeticErrorOperandsExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(arithmetic-error-operands (make-condition 'arithmetic-error :operands '(1 2)))`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestArithmeticErrorOperandsNotArithmeticError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(arithmetic-error-operands (make-condition 'error :operands '(1 2)))`,
		PanicType: slip.Symbol("unbound-slot"),
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTypeErrorDatumExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(type-error-datum (make-condition 'type-error :datum 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestTypeErrorDatumSub(t *testing.T) {
	(&sliptest.Function{
		Source: `(type-error-datum (make-condition 'simple-type-error :datum 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestTypeErrorDatumNotTypeError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(type-error-datum (make-condition 'error :datum 'test))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

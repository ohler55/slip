// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTypeErrorExpectedTypeExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(type-error-expected-type (make-condition 'type-error :expected-type '(fixnum float)))`,
		Expect: "fixnum, float",
	}).Test(t)
}

func TestTypeErrorExpectedTypeSub(t *testing.T) {
	(&sliptest.Function{
		Source: `(type-error-expected-type (make-condition 'simple-type-error :expected-type 'fixnum))`,
		Expect: "fixnum",
	}).Test(t)
}

func TestTypeErrorExpectedTypeNotTypeError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(type-error-expected-type (make-condition 'error :expected-type 'test))`,
		PanicType: slip.Symbol("unbound-slot"),
	}).Test(t)
}

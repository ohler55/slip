// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTypeErrorContextExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(type-error-context (make-condition 'type-error :context 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestTypeErrorContextSub(t *testing.T) {
	(&sliptest.Function{
		Source: `(type-error-context (make-condition 'simple-type-error :context 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestTypeErrorContextNotTypeError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(type-error-context (make-condition 'error :context 'test))`,
		PanicType: slip.Symbol("unbound-slot"),
	}).Test(t)
}

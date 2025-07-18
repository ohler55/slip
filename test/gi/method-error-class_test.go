// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMethodErrorClassExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(method-error-class (make-condition 'invalid-method-error :class 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestMethodErrorClassNotMethodError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(method-error-class (make-condition 'error :class 'test))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

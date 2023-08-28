// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMethodErrorQualifierExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(method-error-qualifier (make-condition 'method-error :qualifier 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestMethodErrorQualifierNotMethodError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(method-error-qualifier (make-condition 'error :qualifier 'test))`,
		PanicType: slip.Symbol("unbound-slot"),
	}).Test(t)
}

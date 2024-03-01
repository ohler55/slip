// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTrimSpaceOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(trim-space " abc \n")`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestTrimSpaceBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(trim-space t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

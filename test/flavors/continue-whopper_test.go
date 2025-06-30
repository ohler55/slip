// Copyright (c) 2025, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestContinueWhopperOutside(t *testing.T) {
	(&sliptest.Function{
		Source:    `(let ((~whopper-location~ t)) (continue-whopper))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

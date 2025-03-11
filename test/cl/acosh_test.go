// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAcoshOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(acosh 1.1)`,
		Expect: "/0.44[0-9]+/",
	}).Test(t)
}

func TestAcoshNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(acosh t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

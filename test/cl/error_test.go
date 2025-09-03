// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestErrorPanic(t *testing.T) {
	(&sliptest.Function{
		Source:    `(error "failed: ~A" 123)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestErrorMessage(t *testing.T) {
	(&sliptest.Function{
		Source: `(recover r r (error "failed: ~A" 123))`,
		Expect: `/#<error [0-9a-f]+>/`,
	}).Test(t)
}

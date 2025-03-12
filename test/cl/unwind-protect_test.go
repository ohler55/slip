// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnwindProtect(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("x", slip.Fixnum(1))
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(unwind-protect (setq x (/ 1 0)) (setq x 3))`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
	tt.Equal(t, slip.Fixnum(3), scope.Get("x"))
}

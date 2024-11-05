// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSlotUnboundOk(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-unbound nil (make-instance 'vanilla-flavor) 'anything)`,
		PanicType: slip.UnboundSlotSymbol,
	}).Test(t)
}

func TestSlotUnboundBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-unbound nil 7 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

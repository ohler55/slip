// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSlotMissingOk(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-missing nil (make-instance 'vanilla-flavor) 'anything 'setf 7)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSlotMissingBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-missing nil 7 t 'slot-boundp)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSlotMissingBadOperation(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-missing nil 7 'anything t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

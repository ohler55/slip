// Copyright (c) 2024, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSlotMissingOk(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-value (make-instance 'vanilla-flavor) 'anything)`,
		PanicType: slip.CellErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (slot-value (make-instance 'vanilla-flavor) 'anything) 7)`,
		PanicType: slip.CellErrorSymbol,
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

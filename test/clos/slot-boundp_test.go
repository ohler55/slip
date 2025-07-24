// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSlotBoundpOk(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor blueberry ((size "medium")) ()
           :initable-instance-variables
           :gettable-instance-variables)
`, scope).Eval(scope, nil)

	(&sliptest.Function{
		Source: `(slot-boundp (make-instance 'blueberry) 'size)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((berry (make-instance 'blueberry)))
                  (slot-makunbound berry 'size)
                  (slot-boundp berry 'size))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(slot-boundp (make-instance 'blueberry) 'not-a-slot)`,
		PanicType: slip.CellErrorSymbol,
	}).Test(t)
}

func TestSlotBoundpBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-boundp 7 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSlotBoundpNoSlot(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-boundp 7 'size)`,
		PanicType: slip.CellErrorSymbol,
	}).Test(t)
}

// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSlotValueGetSet(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor blueberry ((size "medium")) ()
           :initable-instance-variables
           :gettable-instance-variables)
`).Eval(scope, nil)

	(&sliptest.Function{
		Source: `(let ((berry (make-instance 'blueberry :size "medium")))
                  (slot-value berry 'size))`,
		Expect: `"medium"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((berry (make-instance 'blueberry :size "medium")))
                  (setf (slot-value berry 'size) "small")
                  (slot-value berry 'size))`,
		Expect: `"small"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((berry (make-instance 'blueberry :size "medium")))
                  (slot-makunbound berry 'size)
                  (slot-value berry 'size))`,
		PanicType: slip.UnboundSlotSymbol,
	}).Test(t)
}

func TestSlotValueBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-value 7 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (slot-value 7 t) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSlotValueNoSlot(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-value 7 'size)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(slot-value (make-instance 'vanilla-flavor) 'size)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source:    `(setf (slot-value 7 'size) 3)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (slot-value (make-instance 'vanilla-flavor) 'size) 3)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

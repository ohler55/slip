// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSlotMakunboundOk(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor blueberry ((size "medium")) ()
           :initable-instance-variables
           :gettable-instance-variables)
`).Eval(scope, nil)

	(&sliptest.Function{
		Source: `(let ((berry (make-instance 'blueberry)))
                  (slot-makunbound berry 'size)
                  (slot-boundp berry 'size))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(slot-makunbound (make-instance 'blueberry) 'not-a-slot)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSlotMakunboundBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-makunbound 7 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSlotMakunboundNoSlot(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-makunbound 7 'size)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

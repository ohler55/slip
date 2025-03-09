// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSlotExistspOk(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor blueberry ((size "medium")) ()
           :initable-instance-variables
           :gettable-instance-variables)
`, scope).Eval(scope, nil)

	(&sliptest.Function{
		Source: `(slot-exists-p (make-instance 'blueberry) 'size)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(slot-exists-p (make-instance 'blueberry) 'xyz)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(slot-exists-p 7 'xyz)`,
		Expect: "nil",
	}).Test(t)
}

func TestSlotExistspBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-exists-p 7 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

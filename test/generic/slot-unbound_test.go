// Copyright (c) 2024, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSlotUnboundOk(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	(&sliptest.Function{
		Source: `(defclass quux () ((x :initarg :x)))`,
		Expect: `#<standard-class quux>`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux)))
                   (slot-makunbound q 'x)
                   (slot-value q 'x))`,
		PanicType: slip.UnboundSlotSymbol,
	}).Test(t)
}

func TestSlotUnboundBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(slot-unbound nil 7 t)`,
		PanicType: slip.UnboundSlotSymbol,
	}).Test(t)
}

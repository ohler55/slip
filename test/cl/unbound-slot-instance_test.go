// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnboundSlotInstanceExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(unbound-slot-instance (make-condition 'unbound-slot :instance 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestUnboundSlotInstanceNotUnboundSlot(t *testing.T) {
	(&sliptest.Function{
		Source:    `(unbound-slot-instance (make-condition 'error :instance 'test))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

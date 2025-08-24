// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNextMethodPTrue(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (x)
                   (:method :around ((x fixnum)) (if (next-method-p) 2 3))
                   (:method ((x fixnum)) (1+x)))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(quux 0)`,
		Expect: "2",
	}).Test(t)
}

func TestNextMethodPFalse(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (x)
                   (:method :around ((x fixnum)) (if (next-method-p) 2 3)))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(quux 0)`,
		Expect: "3",
	}).Test(t)
}

func TestNextMethodPPanic(t *testing.T) {
	(&sliptest.Function{
		Source:    `(next-method-p)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

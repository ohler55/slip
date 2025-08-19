// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNoNextMethodDefault(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (x)
                   (:method :around ((x fixnum)) (call-next-method 3)))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(quux 0)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestNoNextMethodOverride(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (x)
                   (:method :around ((x fixnum)) (call-next-method 3)))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((m (find-method 'no-next-method '() '(function method))))
                   (when m (remove-method 'no-next-method m))
                   (defmethod no-next-method ((gf t) (m method) &rest args) 7))`,
		Expect: `/#<method no-next-method \(\(gf t\) \(m method\) &rest args\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(quux 0)`,
		Expect: "7",
	}).Test(t)
}

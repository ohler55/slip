// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMethodQualifiersOkay(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (x y)
                   (:method ((x fixnum) (y real)) (+ x y))
                   (:method :before ((x real) (y real)) (print "before"))
                   (:method :after ((x fixnum) (y real)) (print "after"))
                   (:method :around ((x integer) (y real)) (print "around")))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(method-qualifiers (find-method 'quux '() '(fixnum real)))`,
		Expect: `nil`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(method-qualifiers (find-method 'quux '(:before) '(real real)))`,
		Expect: `(:before)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(method-qualifiers (find-method 'quux '(:after) '(fixnum real)))`,
		Expect: `(:after)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(method-qualifiers (find-method 'quux '(:around) '(integer real)))`,
		Expect: `(:around)`,
	}).Test(t)
}

func TestMethodQualifiersNotMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(method-qualifiers t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

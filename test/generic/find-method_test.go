// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFindMethodUsual(t *testing.T) {
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
		Source: `(find-method 'quux '() '(fixnum real))`,
		Expect: `/#<method quux \(\(x fixnum\) \(y real\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-method 'quux nil '(fixnum real))`,
		Expect: `/#<method quux \(\(x fixnum\) \(y real\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-method 'quux '(:before) '(real real))`,
		Expect: `/#<method quux :before \(\(x real\) \(y real\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-method 'quux '(:after) '(fixnum real))`,
		Expect: `/#<method quux :after \(\(x fixnum\) \(y real\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-method 'quux '(:around) '(integer real))`,
		Expect: `/#<method quux :around \(\(x integer\) \(y real\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(find-method 'quux '(:before) '(fixnum real))`,
		Expect: `nil`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(find-method 'quux '(:before) '(fixnum real) t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source:    `(find-method 'quux '(not-a-qualifier) '(fixnum real))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(find-method 'quux '() '(fixnum))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestFindMethodNotGeneric(t *testing.T) {
	(&sliptest.Function{
		Source:    `(find-method 'car '() '(fixnum))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestFindMethodClass(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(let ((q (defgeneric quux (x) (:method ((x fixnum)) (1+ x)))))
                   (find-method q '() (list (find-class 'fixnum))))`,
		Expect: `/#<method quux \(\(x fixnum\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
}

func TestFindMethodTrue(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(let ((q (defgeneric quux (x) (:method ((x t)) (list x)))))
                   (find-method q '() '(t)))`,
		Expect: `/#<method quux \(\(x t\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
}

func TestFindMethodBadSpecializer(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(let ((q (defgeneric quux (x) (:method ((x fixnum)) (1+ x)))))
                   (find-method q '() '(7)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

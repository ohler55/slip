// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRemoveMethodUsual(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	slip.CurrentPackage.Undefine("buux")
	(&sliptest.Function{
		Source: `(defgeneric quux (x y)
                   (:method ((x fixnum) (y real)) (+ x y))
                   (:method :before ((x real) (y real)) (print "before"))
                   (:method :after ((x fixnum) (y real)) (print "after"))
                   (:method :around ((x integer) (y real)) (print "around")))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(defgeneric buux (x y) (:method ((x fixnum) (y real)) (+ x y)))`,
		Expect: "#<generic-function buux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(progn
                   (remove-method 'quux (find-method 'quux '(:before) '(real real)))
                   (pretty-print quux nil))`,
		Expect: `"(defgeneric quux (x y)
  (:method ((x fixnum) (y real))
    (+ x y))
  (:method :after ((x fixnum) (y real))
    (print "after"))
  (:method :around ((x integer) (y real))
    (print "around")))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(progn
                   (remove-method 'quux (find-method 'quux '(:after) '(fixnum real)))
                   (pretty-print quux nil))`,
		Expect: `"(defgeneric quux (x y)
  (:method ((x fixnum) (y real))
    (+ x y))
  (:method :around ((x integer) (y real))
    (print "around")))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(progn
                   (remove-method 'quux (find-method 'quux '(:around) '(integer real)))
                   (pretty-print quux nil))`,
		Expect: `"(defgeneric quux (x y)
  (:method ((x fixnum) (y real))
    (+ x y)))
"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(progn
                   (remove-method 'quux (find-method 'buux '() '(fixnum real)))
                   (pretty-print quux nil))`,
		Expect: `"(defgeneric quux (x y)
  (:method ((x fixnum) (y real))
    (+ x y)))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(progn
                   (remove-method 'quux (find-method 'quux '() '(fixnum real)))
                   (pretty-print quux nil))`,
		Expect: `"(defgeneric quux (x y))
"`,
	}).Test(t)
}

func TestRemoveMethodCover(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (x &optional y) (:method ((x real) &optional y) (list x y)))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(progn
                   (quux 1 2) ;; builds a cached method for "real"
                   (remove-method 'quux (find-method 'quux '() '(real)))
                   (pretty-print quux nil))`,
		Expect: `"(defgeneric quux (x &optional y))
"`,
	}).Test(t)
}

func TestRemoveMethodNotGeneric(t *testing.T) {
	(&sliptest.Function{
		Source:    `(remove-method 'car nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRemoveMethodBadMethod(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(let ((q (defgeneric quux (x) (:method ((x fixnum)) (1+ x)))))
                   (remove-method q 7))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

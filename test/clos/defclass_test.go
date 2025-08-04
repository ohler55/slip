// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefclassMinimal(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	// There is no undef for a class but a new defclass will replace an
	// existing class or at least the fields in the class.
	(&sliptest.Function{
		Source: `(let ((cc (defclass quux () ()))) (pretty-print cc nil))`,
		Expect: `"(defclass quux ()
  ())
"`,
	}).Test(t)
}

func TestDefclassSlots(t *testing.T) {
	for _, name := range []string{"quux",
		"quux-x", "quux-y",
		"quux-set-x", "quux-set-y",
		"quux-acc-x", "quux-acc-y",
		"(setf-acc-x)", "(setf-acc-y)",
	} {
		slip.CurrentPackage.Remove(name)
	}
	(&sliptest.Function{
		Source: `(progn
                   (defclass quux ()
                     ((x :initform 7 :reader quux-x :writer quux-set-x :accessor quux-acc-x :gettable t :settable t)
                      (y :initform 2 :reader quux-y :writer quux-set-y :accessor quux-acc-y :allocation :class)))
                   (let* ((q (make-instance 'quux))
                          (result (list (quux-x q))))
                     (quux-set-x q 3)
                     (addf result (quux-x q))
                     (setf (quux-acc-x q) 4)
                     (addf result (quux-x q))
                     (quux-set-y q 4)
                     (addf result (quux-y q))
                     (setf (quux-acc-y q) 6)
                     (addf result (quux-y q))
                     (send q :set-x 1)
                     (addf result (send q :x))
                     result))`,
		Expect: "(7 3 4 4 6 1)",
	}).Test(t)
}

func TestDefclassUnboundSlot(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	(&sliptest.Function{
		Source: `(progn
                   (defclass quux () ((x :initform 7)))
                   (let ((q (make-instance 'quux)))
                   (slot-makunbound q 'x)
                   (slot-value q 'x)))`,
		PanicType: slip.UnboundSlotSymbol,
	}).Test(t)
}

func TestDefclassSettableError(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	(&sliptest.Function{
		Source: `(progn
                   (defclass quux () ((x :initform 7 :settable t)))
                   (let ((q (make-instance 'quux)))
                     (send q :set-x)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDefclassBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defclass t () ())`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefclassBadSupers(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defclass quux t ())`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defclass quux (t) ())`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defclass quux (vanilla-flavor) ())`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefclassBadSlots(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defclass quux () t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefclassBadClassOption(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defclass quux () () (:what-is-this 7))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defclass quux () () (:documentation))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defclass quux () () (:default-initargs :x))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defclass quux () () (:default-initargs t t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefclassBadDocs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defclass quux () () (:documentation t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefclassNoRedefine(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defclass fixnum () ())`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

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

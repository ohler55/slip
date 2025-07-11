// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefineConditionMinimal(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bc (define-condition base-condition () ()))) (pretty-print bc nil))`,
		Expect: `"(define-condition base-condition ()
  ())
"`,
	}).Test(t)
}

func TestDefineConditionBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(define-condition t () ())`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefineConditionBadSupers(t *testing.T) {
	(&sliptest.Function{
		Source:    `(define-condition quux t ())`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(define-condition quux (t) ())`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(defclass not-condition () ())`,
		Expect: "#<standard-class not-condition>",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(define-condition quux (not-condition) ())`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefineConditionBadSlots(t *testing.T) {
	(&sliptest.Function{
		Source:    `(define-condition quux () t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefineConditionBadClassOption(t *testing.T) {
	(&sliptest.Function{
		Source:    `(define-condition quux () () (:what-is-this 7))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(define-condition quux () () (:documentation))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(define-condition quux () () (:default-initargs :x))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(define-condition quux () () (:default-initargs t t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefineConditionBadDocs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(define-condition quux () () (:documentation t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

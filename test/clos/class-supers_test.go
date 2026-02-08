// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClassSupersFlavor(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-supers vanilla-flavor)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(class-supers 'bag-flavor)`,
		Expect: "(vanilla-flavor)",
	}).Test(t)
}

func TestClassSupersStandard(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	slip.CurrentPackage.Remove("buux")

	(&sliptest.Function{
		Source: `(progn (defclass buux () ()) (defclass quux (buux) ()))`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(class-supers 'quux)`,
		Expect: "(buux)",
	}).Test(t)
}

func TestClassSupersCondition(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	slip.CurrentPackage.Remove("buux")

	(&sliptest.Function{
		Source: `(progn (define-condition buux () ()) (define-condition quux (buux) ()))`,
		Expect: "#<condition-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(class-supers 'quux)`,
		Expect: "(buux)",
	}).Test(t)
}

func TestClassSuperAnyClass(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn (defstruct baax x) (defstruct (qaax (:include baax))))`,
		Expect: "qaax",
	}).Test(t)
	(&sliptest.Function{
		Source: `(class-supers 'qaax)`,
		Expect: "baax",
	}).Test(t)
}

func TestClassSupersBuiltIn(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-supers 'fixnum)`,
		Expect: "(integer)",
	}).Test(t)
}

func TestClassSupersNotClass(t *testing.T) {
	(&sliptest.Function{
		Source:    `(class-supers t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

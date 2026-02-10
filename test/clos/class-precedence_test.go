// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClassPrecedenceFlavor(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-precedence vanilla-flavor)`,
		Expect: "(vanilla-flavor instance t)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(class-precedence 'bag-flavor)`,
		Expect: "(bag-flavor vanilla-flavor instance t)",
	}).Test(t)
}

func TestClassPrecedenceStandard(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	slip.CurrentPackage.Remove("buux")

	(&sliptest.Function{
		Source: `(progn (defclass buux () ()) (defclass quux (buux) ()))`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(class-precedence 'quux)`,
		Expect: "(quux buux standard-object t)",
	}).Test(t)
}

func TestClassPrecedenceCondition(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	slip.CurrentPackage.Remove("buux")

	(&sliptest.Function{
		Source: `(progn (define-condition buux () ()) (define-condition quux (buux) ()))`,
		Expect: "#<condition-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(class-precedence 'quux)`,
		Expect: "(quux buux condition t)",
	}).Test(t)
}

func TestClassPrecedenceBuiltIn(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-precedence 'fixnum)`,
		Expect: "(fixnum integer rational real number t)",
	}).Test(t)
}

func TestClassPrecedenceNotClass(t *testing.T) {
	(&sliptest.Function{
		Source:    `(class-precedence t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

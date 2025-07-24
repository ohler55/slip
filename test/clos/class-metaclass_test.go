// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClassMetaclassFlavor(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-metaclass vanilla-flavor)`,
		Expect: "flavor",
	}).Test(t)
	(&sliptest.Function{
		Source: `(class-metaclass 'vanilla-flavor)`,
		Expect: "flavor",
	}).Test(t)
}

func TestClassMetaclassBuiltIn(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-metaclass 'fixnum)`,
		Expect: "built-in-class",
	}).Test(t)
}

func TestClassMetaclassCondition(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-metaclass (find-class 'serious-condition))`,
		Expect: "condition-class",
	}).Test(t)
}

func TestClassMetaclassStandard(t *testing.T) {
	slip.CurrentPackage.Remove("quux")

	(&sliptest.Function{
		Source: `(defclass quux () ())`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(class-metaclass 'quux)`,
		Expect: "standard-class",
	}).Test(t)
}

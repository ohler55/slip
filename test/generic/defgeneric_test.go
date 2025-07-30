// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefgenericUsual(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (a b)
                   (:documentation "quack")
                   (:method-class standard-method)
                   (:method ((a fixnum) (b fixnum)) (+ a b)))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(quux 1 2)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(quux 1 2.1)`,
		PanicType: slip.NoApplicableMethodErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(quux 1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDefgenericAux(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (a &optional b)
                   (:method ((a fixnum) &optional b) (list 'fix a b))
                   (:method ((a t) &optional b) (list 'any a b))
)`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(quux 1 2)`,
		Expect: "(fix 1 2)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(quux nil 1)`,
		Expect: "(any nil 1)",
	}).Test(t)
}

func TestDefgenericBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defgeneric "quux" (a b))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefgenericOrdinaryError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defgeneric defmethod (a b))`,
		PanicType: slip.ProgramErrorSymbol,
	}).Test(t)
}

func TestDefgenericBadLambdaList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defgeneric quux t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defgeneric quux (t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefgenericBadOptionList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defgeneric quux (a b) (:documentation))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defgeneric quux (a b) (:duck duck))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

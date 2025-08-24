// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeLoadFormSymbolFunction(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(progn
                   (defgeneric quux (x y)
                     (:method ((x fixnum) (y real)) (+ x y)))
                   (make-load-form 'quux))`,
		Expect: "(defgeneric quux (x y) (:method ((x fixnum) (y real)) (+ x y)))",
	}).Test(t)
}

func TestMakeLoadFormSymbolClass(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-load-form 'vanilla-flavor)`,
		Expect: `(defflavor vanilla-flavor () ()
           (:documentation "A Flavor that implements the standard methods."))`,
	}).Test(t)
}

func TestMakeLoadFormSymbolVar(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-load-form '*print-base*)`,
		Expect: `10`,
	}).Test(t)
}

func TestMakeLoadFormInstance(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-load-form (make-instance 'vanilla-flavor))`,
		Expect: `(let ((inst (make-instance (quote vanilla-flavor)))) inst)`,
	}).Test(t)
}

func TestMakeLoadFormCondition(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-load-form (make-condition 'Condition))`,
		Expect: `(let ((inst (make-condition (quote condition)))) inst)`,
	}).Test(t)
}

func TestMakeLoadFormPanic(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-load-form (make-string-output-stream))`,
		PanicType: slip.PrintNotReadableSymbol,
	}).Test(t)
}

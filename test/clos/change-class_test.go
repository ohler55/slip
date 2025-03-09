// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestChangeClassSymbol(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size))
(defflavor blackberry ((size "medium") (fresh nil)) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size fresh))
(setq berry (make-instance 'strawberry :size "medium"))
`, scope)
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'strawberry)", scope).Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'blackberry)", scope).Eval(scope, nil)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(change-class berry 'blackberry)`,
		Expect: "/#<blackberry [0-9a-f]+>/",
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(change-class berry 'none-berry)`,
		PanicType: slip.ClassNotFoundSymbol,
	}).Test(t)
}

func TestChangeClassFlavor(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size))
(defflavor blackberry ((size "medium") (fresh nil)) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size fresh))
(setq berry (make-instance 'strawberry :size "medium"))
`, scope)
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'strawberry)", scope).Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'blackberry)", scope).Eval(scope, nil)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(change-class berry (find-class 'blackberry))`,
		Expect: "/#<blackberry [0-9a-f]+>/",
	}).Test(t)
}

func TestChangeClassNotInstance(t *testing.T) {
	(&sliptest.Function{
		Source:    `(change-class 7 'vanilla-flavor)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefmethodFlavors(t *testing.T) {
	defer undefFlavor("berry")
	(&sliptest.Function{
		Source: `(progn
                   (defflavor berry (color) ()
                                    :gettable-instance-variables
                                    :settable-instance-variables
                                    :initable-instance-variables)
                   (defmethod (berry :rot) () "When berries rot they turn brown." (setq color 'brown)))`,
		Expect: "nil",
	}).Test(t)
}

func TestDefmethodCLOS(t *testing.T) {
	(&sliptest.Function{
		Source: `(defmethod rot ((b berry)) (setq color 'brown))`,
		Panics: true,
	}).Test(t)
}

func TestDefmethodBadMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defmethod 7 () (setq color 'brown))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

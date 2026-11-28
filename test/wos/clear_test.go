// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClear(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (setf (wos-get wos 3) 'three)
                   (wos-clear wos)
                   (wos-get wos 3))`,
		Expect: "nil",
	}).Test(t)
}

func TestClearNotWos(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wos-clear t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestClearMethod(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (setf (wos-get wos 3) 'three)
                   (send wos :clear)
                   (send wos :get 3))`,
		Expect: "nil",
	}).Test(t)
}

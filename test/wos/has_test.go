// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestHas(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (setf (wos-get wos 3) 'three)
                   (setf (wos-get wos 2) 'two)
                   (list (wos-has wos 2) (wos-has wos 5)))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestHasNotWos(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wos-has t 1)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestHasBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wos-has (make-wos) 1.5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestHasMethod(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (setf (wos-get wos 3) 'three)
                   (setf (wos-get wos 2) 'one)
                   (list (send wos :has 2) (send wos :has 5)))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestHasMethodBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    "(send (make-wos) :has 1.5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

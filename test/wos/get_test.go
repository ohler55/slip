// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGet(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (setf (wos-get wos 3) 'three)
                   (setf (wos-get wos 2) 'two)
                   (setf (wos-get wos 5) 'five)
                   (list (wos-get wos 2) (wos-get wos 3) (wos-get wos 4) (wos-get wos 5)))`,
		Expect: "(two three nil five)",
	}).Test(t)
}

func TestGetNotWos(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wos-get t 1)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(setf (wos-get t 1) 3)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestGetBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wos-get (make-wos) 1.5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(setf (wos-get (make-wos) 1.5) 3)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestGetMethod(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (setf (wos-get wos 3) 'three)
                   (setf (wos-get wos 2) 'one)
                   (setf (wos-get wos 2) 'two)
                   (setf (wos-get wos 5) 'five)
                   (list (send wos :get 2) (send wos :get 3) (send wos :get 4) (send wos :get 5)))`,
		Expect: "(two three nil five)",
	}).Test(t)
}

func TestGetMethodBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    "(send (make-wos) :get 1.5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

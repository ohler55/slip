// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRemove(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (wos-set wos 3 'three)
                   (wos-set wos 2 'two)
                   (wos-set wos 5 'five)
                   (wos-remove wos 2)
                   (wos-remove wos 5)
                   (list (wos-get wos 2) (wos-get wos 3) (wos-get wos 5)))`,
		Expect: "(nil three nil)",
	}).Test(t)
}

func TestRemoveNotWos(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wos-remove t 1)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRemoveBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wos-remove (make-wos) 1.5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRemoveMethod(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (send wos :set 3 'three)
                   (send wos :set 2 'one)
                   (send wos :set 5 'five)
                   (send wos :remove 2)
                   (send wos :remove 5)
                   (list (send wos :get 2) (send wos :get 3) (send wos :get 5) (length wos)))`,
		Expect: "(nil three nil 1)",
	}).Test(t)
}

func TestRemoveMethodBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    "(send (make-wos) :remove 1.5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

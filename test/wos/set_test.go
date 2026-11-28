// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSet(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (wos-set wos 3 'three)
                   (wos-set wos 2 'one)
                   (wos-set wos 5 'five)
                   (wos-set wos 2 'two)
                   (list (wos-get wos 2) (wos-get wos 3) (wos-get wos 4) (wos-get wos 5)))`,
		Expect: "(two three nil five)",
	}).Test(t)
}

func TestSetNotWos(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wos-set t 1 'x)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSetBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wos-set (make-wos) 1.5 'x)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSetMethod(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wos (make-wos)))
                   (send wos :set 3 'three)
                   (send wos :set 2 'one)
                   (send wos :set 5 'five)
                   (send wos :set 2 'two)
                   (list (send wos :get 2) (send wos :get 3) (send wos :get 4) (send wos :get 5) (length wos)))`,
		Expect: "(two three nil five 3)",
	}).Test(t)
}

func TestSetMethodBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    "(send (make-wos) :set 1.5 'x)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

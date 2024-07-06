// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeListDefault(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-list 3)`,
		Expect: "(nil nil nil)",
	}).Test(t)
}

func TestMakeListInitialElement(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-list 3 :initial-element 'slip)`,
		Expect: "(slip slip slip)",
	}).Test(t)
}

func TestMakeListBadSize(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-list t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

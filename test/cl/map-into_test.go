// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMapIntoFull(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq '(a b c))) (list (map-into seq '+ '(1 2 3) '(10 20 30 40)) seq))`,
		Expect: "((11 22 33) (11 22 33))",
	}).Test(t)
}

func TestMapIntoPartial(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq '(a b c))) (list (map-into seq '+ '(1 2) '(10 20 30 40)) seq))`,
		Expect: "((11 22 c) (11 22 c))",
	}).Test(t)
}

func TestMapIntoBadResultSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(map-into t '+ '(1 2) '(10 20 30 40))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMapIntoNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(map-into '(a b c) '+ t '(10 20 30 40))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

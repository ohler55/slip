// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPairlisNew(t *testing.T) {
	(&sliptest.Function{
		Source: `(pairlis '(a b) '(0 1))`,
		Expect: "((a . 0) (b . 1))",
	}).Test(t)
}

func TestPairlisPrepend(t *testing.T) {
	(&sliptest.Function{
		Source: `(pairlis '(a b) '(0 1) '((c . 2)))`,
		Expect: "((a . 0) (b . 1) (c . 2))",
	}).Test(t)
}

func TestPairlisKeysNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(pairlis t '(0 1) '((c . 2)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestPairlisValuesNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(pairlis '(a b) t '((c . 2)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestPairlisAlistNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(pairlis '(a b) '(0 1) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestPairlisCountUnequal(t *testing.T) {
	(&sliptest.Function{
		Source:    `(pairlis '(a b) '(0 1 2))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

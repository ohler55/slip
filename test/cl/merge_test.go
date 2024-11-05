// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMergeList(t *testing.T) {
	(&sliptest.Function{
		Source: `(merge 'list '(1 2 3) '(0 3 4 5) nil)`,
		Expect: "(0 1 2 3 3 4 5)",
	}).Test(t)
}

func TestMergeVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(merge 'vector #(1 2 3) '(0 3 4 5) nil)`,
		Array:  true,
		Expect: "#(0 1 2 3 3 4 5)",
	}).Test(t)
}

func TestMergeString(t *testing.T) {
	(&sliptest.Function{
		Source: `(merge 'string '(#\A #\B #\C) '(#\D #\E) #'< :key #'char-code)`,
		Expect: `"ABCDE"`,
	}).Test(t)
}

func TestMergeOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(merge 'octets '(1 2 6 7) '(3 4 5) nil)`,
		Array:  true,
		Expect: "#(1 2 3 4 5 6 7)",
	}).Test(t)
}

func TestMergeBadResultType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(merge 'fixnum '(1 2 3) '(0 3 4 5) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(merge t '(1 2 3) '(0 3 4 5) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

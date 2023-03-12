// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMapcanLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcan (lambda (x y) (list x y)) '(a b c) '(1 2 3))`,
		Expect: "(a 1 b 2 c 3)",
	}).Test(t)
}

func TestMapcanNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcan 'print t)`,
		Panics: true,
	}).Test(t)
}

func TestMapcanNotList2(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcan 'print '(1 2) t)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMapconLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcon (lambda (x) (list x)) '(2 3 4))`,
		Expect: "((2 3 4) (3 4) (4))",
	}).Test(t)
}

func TestMapconMultiple(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcon 'list '(1 2 3 4) '(2 4 6))`,
		Expect: "((1 2 3 4) (2 4 6) (2 3 4) (4 6) (3 4) (6))",
	}).Test(t)
}

func TestMapconNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcon 'print t)`,
		Panics: true,
	}).Test(t)
}

func TestMapconNotList2(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcon 'print '(1 2) t)`,
		Panics: true,
	}).Test(t)
}

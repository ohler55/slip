// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMapcarLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcar (lambda (x) (1+ x)) '(1 2 3))`,
		Expect: "(2 3 4)",
	}).Test(t)
}

func TestMapcarMultiple(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcar '+ '(1 2 3 4) '(2 4 6))`,
		Expect: "(3 6 9)",
	}).Test(t)
}

func TestMapcarNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcar 'print t)`,
		Panics: true,
	}).Test(t)
}

func TestMapcarNotList2(t *testing.T) {
	(&sliptest.Function{
		Source: `(mapcar 'print '(1 2) t)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMaplistLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(maplist (lambda (x) (car x)) '(1 2 3))`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestMaplistMultiple(t *testing.T) {
	(&sliptest.Function{
		Source: `(maplist 'list '(1 2 3 4) '(2 4 6))`,
		Expect: "(((1 2 3 4) (2 4 6)) ((2 3 4) (4 6)) ((3 4) (6)))",
	}).Test(t)
}

func TestMaplistNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(maplist 'print t)`,
		Panics: true,
	}).Test(t)
}

func TestMaplistNotList2(t *testing.T) {
	(&sliptest.Function{
		Source: `(maplist 'print '(1 2) t)`,
		Panics: true,
	}).Test(t)
}

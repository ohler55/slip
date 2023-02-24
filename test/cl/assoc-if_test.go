// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAssocIfBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if (lambda (k) (equal 'x k)) '((x . 1) (y . 2) (z . 3)))",
		Expect: "(x . 1)",
	}).Test(t)
}

func TestAssocIfWithNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if (lambda (k) (equal 'z k)) '((x 1) nil (z . 3)))",
		Expect: "(z . 3)",
	}).Test(t)
}

func TestAssocIfKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if 'zerop '((1 . x) (2 . y) (3 . z)) :key '1-)",
		Expect: "(1 . x)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(assoc-if 'zerop '((1 . x) (2 . y) (3 . z)) :key (lambda (k) (- k 2)))",
		Expect: "(2 . y)",
	}).Test(t)
}

func TestAssocIfNotAlist(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if 'zerop t)",
		Panics: true,
	}).Test(t)
}

func TestAssocIfAlistMember(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if 'zerop '(a b c))",
		Panics: true,
	}).Test(t)
}

func TestAssocIfBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if 'zerop '((a . 1)) t 1)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(assoc-if 'zerop '((a . 1)) :bad 1)",
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAssocIfNotBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if-not (lambda (k) (equal 'x k)) '((x . 1) (y . 2) (z . 3)))",
		Expect: "(y . 2)",
	}).Test(t)
}

func TestAssocIfNotWithNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if-not (lambda (k) (equal 'x k)) '((x 1) nil (z . 3)))",
		Expect: "(z . 3)",
	}).Test(t)
}

func TestAssocIfNotKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if-not 'zerop '((1 . x) (2 . y) (3 . z)) :key '1-)",
		Expect: "(2 . y)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(assoc-if-not 'zerop '((1 . x) (2 . y) (3 . z)) :key (lambda (k) (- k 1)))",
		Expect: "(2 . y)",
	}).Test(t)
}

func TestAssocIfNotNotAlist(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if-not 'zerop t)",
		Panics: true,
	}).Test(t)
}

func TestAssocIfNotAlistMember(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if-not 'zerop '(a b c))",
		Panics: true,
	}).Test(t)
}

func TestAssocIfNotBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc-if-not 'zerop '((a . 1)) t 1)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(assoc-if-not 'zerop '((a . 1)) :bad 1)",
		Panics: true,
	}).Test(t)
}

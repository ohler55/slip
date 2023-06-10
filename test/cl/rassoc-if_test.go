// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRassocIfBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc-if (lambda (k) (equal 'x k)) '((1 . x) (2 . y) (3 . z)))",
		Expect: "(1 . x)",
	}).Test(t)
}

func TestRassocIfWithNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc-if (lambda (k) (equal 'z k)) '((1 . x) nil (3 . z)))",
		Expect: "(3 . z)",
	}).Test(t)
}

func TestRassocIfKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc-if 'zerop '((x . 1) (y . 2) (z . 3)) :key '1-)",
		Expect: "(x . 1)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(rassoc-if 'zerop '((x . 1) (y . 2) (z . 3)) :key (lambda (k) (- k 2)))",
		Expect: "(y . 2)",
	}).Test(t)
}

func TestRassocIfNotAlist(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc-if 'zerop t)",
		Panics: true,
	}).Test(t)
}

func TestRassocIfAlistMember(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc-if 'zerop '(a b c))",
		Panics: true,
	}).Test(t)
}

func TestRassocIfBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc-if 'zerop '((a . 1)) t 1)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(rassoc-if 'zerop '((a . 1)) :bad 1)",
		Panics: true,
	}).Test(t)
}

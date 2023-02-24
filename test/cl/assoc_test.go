// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAssocBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc 'x '((x . 1) (y . 2) (z . 3)))",
		Expect: "(x . 1)",
	}).Test(t)
}

func TestAssocWithNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc 'z '((x 1) nil (z . 3)))",
		Expect: "(z . 3)",
	}).Test(t)
}

func TestAssocKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc 3 '((1 . x) (2 . y) (3 . z)) :key '1+)",
		Expect: "(2 . y)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(assoc 3 '((1 . x) (2 . y) (3 . z)) :key (lambda (k) (1+ k)))",
		Expect: "(2 . y)",
	}).Test(t)
}

func TestAssocTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc 2 '((1 . x) (2 . y) (3 . z)) :test (lambda (item key) (equal item key)))",
		Expect: "(2 . y)",
	}).Test(t)
}

func TestAssocNotAlist(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc 'x t)",
		Panics: true,
	}).Test(t)
}

func TestAssocAlistMember(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc 'x '(a b c))",
		Panics: true,
	}).Test(t)
}

func TestAssocBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(assoc 'x '((a . 1)) t 1)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(assoc 'x '((a . 1)) :bad 1)",
		Panics: true,
	}).Test(t)
}

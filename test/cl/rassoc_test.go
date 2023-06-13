// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRassocBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc 1 '((x . 1) (y . 2) (z . 3)))",
		Expect: "(x . 1)",
	}).Test(t)
}

func TestRassocWithNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc 3 '((x 1) nil (z . 3)))",
		Expect: "(z . 3)",
	}).Test(t)
}

func TestRassocKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc 3 '((x . 1) (y . 2) (z . 3)) :key '1+)",
		Expect: "(y . 2)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(rassoc 3 '((x . 1) (y . 2) (z . 3)) :key (lambda (k) (1+ k)))",
		Expect: "(y . 2)",
	}).Test(t)
}

func TestRassocTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc 2 '((x . 1) (y . 2) (z . 3)) :test (lambda (item key) (equal item key)))",
		Expect: "(y . 2)",
	}).Test(t)
}

func TestRassocNotAlist(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc 'x t)",
		Panics: true,
	}).Test(t)
}

func TestRassocAlistMember(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc 'x '(a b c))",
		Panics: true,
	}).Test(t)
}

func TestRassocBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(rassoc 'x '((a . 1)) t 1)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(rassoc 'x '((a . 1)) :bad 1)",
		Panics: true,
	}).Test(t)
}

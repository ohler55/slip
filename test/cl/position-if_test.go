// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPositionIfEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if 'numberp nil)",
		Expect: "nil",
	}).Test(t)
}

func TestPositionIfListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if 'numberp '(a 2 3))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position-if 'numberp '(a b c))",
		Expect: "nil",
	}).Test(t)
}

func TestPositionIfVectorPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if 'numberp #(a 2 c))",
		Expect: "1",
	}).Test(t)
}

func TestPositionIfStringPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(position-if (lambda (c) (< 66 (char-code c))) "ABCD")`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position-if (lambda (c) (< 68 (char-code c))) "ABCD")`,
		Expect: "nil",
	}).Test(t)
}

func TestPositionIfNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if 'numberp t)",
		Panics: true,
	}).Test(t)
}

func TestPositionIfListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if 'numberp '(1 b 3 d e) :start 1 :end 3)",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position-if 'numberp '(a b c 1 2) :start 1 :end 3)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position-if 'numberp '(1 2 3 4 5) :start 5 :end nil)",
		Expect: "nil",
	}).Test(t)
}

func TestPositionIfBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if 'numberp '(a b c) :start t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(position-if 'numberp '(a b c) :start -1)",
		Panics: true,
	}).Test(t)
}

func TestPositionIfStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(position-if (lambda (c) (< 66 (char-code c))) "ABCDE" :start 1 :end 3)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position-if (lambda (c) (< 67 (char-code c))) "ABCDE" :start 1 :end 3)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position-if (lambda (c) (< 66 (char-code c))) "ABCDE" :start 5 :end nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestPositionIfListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if 'numberp '((a 1) (2 b) (c 3) (4 b)) :key 'car)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position-if 'numberp '((a 1) (2 b) (c 3) (4 b)) :key 'car :from-end t)",
		Expect: "3",
	}).Test(t)
}

func TestPositionIfStringKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(position-if (lambda (n) (< 65 n)) "ABC" :key 'char-code)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position-if (lambda (n) (< 65 n)) "ABC" :key 'char-code :from-end t)`,
		Expect: "2",
	}).Test(t)
}

func TestPositionIfBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if 'numberp '(a b c) :end t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(position-if 'numberp '(a b c) :end -1)",
		Panics: true,
	}).Test(t)
}

func TestPositionIfBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if 'numberp '(a b c) t 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(position-if 'numberp '(a b c) :bad 3)",
		Panics: true,
	}).Test(t)
}

func TestPositionIfBadPredicate(t *testing.T) {
	(&sliptest.Function{
		Source: "(position-if t '(a b c))",
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFindIfEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if 'numberp nil)",
		Expect: "nil",
	}).Test(t)
}

func TestFindIfListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if 'numberp '(a 2 3))",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find-if 'numberp '(a b c))",
		Expect: "nil",
	}).Test(t)
}

func TestFindIfVectorPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if 'numberp #(a 2 c))",
		Expect: "2",
	}).Test(t)
}

func TestFindIfStringPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 66 (char-code c))) "ABCD")`,
		Expect: "#\\C",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 68 (char-code c))) "ABCD")`,
		Expect: "nil",
	}).Test(t)
}

func TestFindIfOctetsPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 66 c)) (coerce "ABCD" 'octets))`,
		Expect: "67",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 68 c)) (coerce "ABCD" 'octets))`,
		Expect: "nil",
	}).Test(t)
}

func TestFindIfNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if 'numberp t)",
		Panics: true,
	}).Test(t)
}

func TestFindIfListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if 'numberp '(1 b 3 d e) :start 1 :end 3)",
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find-if 'numberp '(a b c 1 2) :start 1 :end 3)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find-if 'numberp '(1 2 3 4 5) :start 5 :end nil)",
		Expect: "nil",
	}).Test(t)
}

func TestFindIfBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if 'numberp '(a b c) :start t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(find-if 'numberp '(a b c) :start -1)",
		Panics: true,
	}).Test(t)
}

func TestFindIfStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 66 (char-code c))) "ABCDE" :start 1 :end 3)`,
		Expect: "#\\C",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 67 (char-code c))) "ABCDE" :start 1 :end 3)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 66 (char-code c))) "ABCDE" :start 5 :end nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestFindIfOctetsStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 66 c)) (coerce "ABCDE" 'octets) :start 1 :end 3)`,
		Expect: "67",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 67 c)) (coerce "ABCDE" 'octets) :start 1 :end 3)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-if (lambda (c) (< 66 c)) (coerce "ABCDE" 'octets) :start 5 :end nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestFindIfListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if 'numberp '((a 1) (2 b) (c 3) (4 b)) :key 'car)",
		Expect: "(2 b)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find-if 'numberp '((a 1) (2 b) (c 3) (4 b)) :key 'car :from-end t)",
		Expect: "(4 b)",
	}).Test(t)
}

func TestFindIfStringKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-if (lambda (n) (< 65 n)) "ABC" :key 'char-code)`,
		Expect: "#\\B",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-if (lambda (n) (< 65 n)) "ABC" :key 'char-code :from-end t)`,
		Expect: "#\\C",
	}).Test(t)
}

func TestFindIfOctetsKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-if (lambda (n) (< 66 n)) (coerce "ABC" 'octets) :key (lambda (x) (1+ x)))`,
		Expect: "66",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-if (lambda (n) (< 65 n)) (coerce "ABC" 'octets) :key (lambda (x) (1+ x)) :from-end t)`,
		Expect: "67",
	}).Test(t)
}

func TestFindIfBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if 'numberp '(a b c) :end t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(find-if 'numberp '(a b c) :end -1)",
		Panics: true,
	}).Test(t)
}

func TestFindIfBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if 'numberp '(a b c) t 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(find-if 'numberp '(a b c) :bad 3)",
		Panics: true,
	}).Test(t)
}

func TestFindIfBadPredicate(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-if t '(a b c))",
		Panics: true,
	}).Test(t)
}

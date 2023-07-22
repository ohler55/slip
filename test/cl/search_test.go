// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSearchNilNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(search nil nil)",
		Expect: "0",
	}).Test(t)
}

func TestSearchNilList(t *testing.T) {
	(&sliptest.Function{
		Source: "(search nil '(a b c d))",
		Expect: "0",
	}).Test(t)
}

func TestSearchListNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(search '(b c) nil)",
		Expect: "nil",
	}).Test(t)
}

func TestSearchListList(t *testing.T) {
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(d e) '(a b c d))",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b x) '(a b c d))",
		Expect: "nil",
	}).Test(t)
}

func TestSearchVector(t *testing.T) {
	(&sliptest.Function{
		Source: "(search #(b c) #(a b c d))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search #(b c) '(a b c d))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b c) #(a b c d))",
		Expect: "1",
	}).Test(t)
}

func TestSearchStringList(t *testing.T) {
	(&sliptest.Function{
		Source: `(search "" '(a b c d))`,
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(search "a" '(a b c d))`,
		Expect: "nil",
	}).Test(t)
}

func TestSearchListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(search '((b . 1) (c . 4)) '((a . 1) (b . 2) (c . 3) (d . 4)) :key 'car)",
		Expect: "1",
	}).Test(t)
}

func TestSearchListTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(search '(3 4) '(2 4 3 5 6) :test '<)",
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(3 4) '(2 4 3 5) :test '<)",
		Expect: "nil",
	}).Test(t)
}

func TestSearchListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(search '(a b c d) '(x y b c z w) :start1 1 :end1 3 :start2 1 :end2 5)",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(a b c) '(x y b c z w) :start1 1 :end1 nil :start2 1 :end2 nil)",
		Expect: "2",
	}).Test(t)
}

func TestSearchListOutOfBounds(t *testing.T) {
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d) :start1 -1)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d) :start1 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d) :start2 -1)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d) :start2 5)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d) :end1 -1)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d) :end1 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d) :end2 -1)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d) :end2 5)",
		Panics: true,
	}).Test(t)
}

/////

func TestSearchNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: `(search nil 7)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(search 7 '(a b c))`,
		Panics: true,
	}).Test(t)
}

func TestSearchBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(search nil '(a b c) :bad)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(search nil '(a b c) :start t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(search nil '(a b c) t t)`,
		Panics: true,
	}).Test(t)
}

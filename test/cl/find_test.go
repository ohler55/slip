// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFindEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 1 nil)",
		Expect: "nil",
	}).Test(t)
}

func TestFindListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 'b '(a b c))",
		Expect: "b",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 'x '(a b c))",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 3 '(a b c))",
		Expect: "nil",
	}).Test(t)
}

func TestFindVectorPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 'b #(a b c))",
		Expect: "b",
	}).Test(t)
}

func TestFindStringPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(find #\b "abc")`,
		Expect: "#\\b",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find #\x "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find 3 "abc")`,
		Expect: "nil",
	}).Test(t)
}

func TestFindNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 'b t)",
		Panics: true,
	}).Test(t)
}

func TestFindListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 'c '(a b c d e) :start 1 :end 3)",
		Expect: "c",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 'd '(a b c d e) :start 1 :end 3)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 'd '(a b c d e) :start 5 :end nil)",
		Expect: "nil",
	}).Test(t)
}

func TestFindBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 'b '(a b c) :start t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 'b '(a b c) :start -1)",
		Panics: true,
	}).Test(t)
}

func TestFindStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(find #\c "abcde" :start 1 :end 3)`,
		Expect: "#\\c",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find #\d "abcde" :start 1 :end 3)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find #\d "abcde" :start 5 :end nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestFindListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 'b '((a 1) (b 2) (c 3) (b 4)) :key 'car)",
		Expect: "(b 2)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 'b '((a 1) (b 2) (c 3) (b 4)) :key 'car :from-end t)",
		Expect: "(b 4)",
	}).Test(t)
}

func TestFindStringKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(find 66 "ABC" :key 'char-code)`,
		Expect: "#\\B",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find 66 "ABC" :key 'char-code :from-end t)`,
		Expect: "#\\B",
	}).Test(t)
}

func TestFindListTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 2 '(1 2 3 4) :test '<)",
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 2 '(1 2 3 4) :test '< :from-end t)",
		Expect: "4",
	}).Test(t)
}

func TestFindStringTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(find 66 "ABCD" :test '< :key 'char-code)`,
		Expect: "#\\C",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find 66 "ABCD" :test '< :key 'char-code :from-end t)`,
		Expect: "#\\D",
	}).Test(t)
}

func TestFindBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 'b '(a b c) :end t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 'b '(a b c) :end -1)",
		Panics: true,
	}).Test(t)
}

func TestFindBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(find 'b '(a b c) t 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 'b '(a b c) :bad 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(find 'b '(a b c) :count 3)",
		Panics: true,
	}).Test(t)
}

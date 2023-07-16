// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDeleteDuplicatesNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates nil)",
		Expect: "nil",
	}).Test(t)
}

func TestDeleteDuplicatesListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 2 3 2 1))",
		Expect: "(3 2 1)",
	}).Test(t)
}

func TestDeleteDuplicatesListFromEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 2 3 2 1) :from-end t)",
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestDeleteDuplicatesListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 2 3 2 1) :start 1 :end 4)",
		Expect: "(1 3 2 1)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 2 3 2 1) :start 1 :end nil)",
		Expect: "(1 3 2 1)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 2 3 2 1) :start 1 :end 4 :from-end t)",
		Expect: "(1 2 3 1)",
	}).Test(t)
}

func TestDeleteDuplicatesListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 2 3 2 1) :key 'evenp)",
		Expect: "(2 1)",
	}).Test(t)
}

func TestDeleteDuplicatesListTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 2 3 2 1) :test (lambda (v0 v1) (equal (evenp v0) (evenp v1))))",
		Expect: "(2 1)",
	}).Test(t)
}

func TestDeleteDuplicatesVectorPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates #(1 2 3 2 1))",
		Expect: "#(3 2 1)",
	}).Test(t)
}

func TestDeleteDuplicatesStringPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-duplicates "abcba")`,
		Expect: `"cba"`,
	}).Test(t)
}

func TestDeleteDuplicatesStringFromEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-duplicates "abcba" :from-end t)`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestDeleteDuplicatesStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-duplicates "abcba" :start 1 :end 4)`,
		Expect: `"acba"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete-duplicates "abcba" :start 1 :end nil)`,
		Expect: `"acba"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete-duplicates "abcba" :start 1 :end 4 :from-end t)`,
		Expect: `"abca"`,
	}).Test(t)
}

func TestDeleteDuplicatesStringKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-duplicates "abcba" :key 'char-code)`,
		Expect: `"cba"`,
	}).Test(t)
}

func TestDeleteDuplicatesStringTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-duplicates "abcba" :test (lambda (x y) (equal x y)))`,
		Expect: `"cba"`,
	}).Test(t)
}

func TestDeleteDuplicatesNotSeq(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates t)",
		Panics: true,
	}).Test(t)
}

func TestDeleteDuplicatesBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 1) t t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 1) :bad 1)",
		Panics: true,
	}).Test(t)
}

func TestDeleteDuplicatesBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 1) :start t)",
		Panics: true,
	}).Test(t)
}

func TestDeleteDuplicatesBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-duplicates '(1 1) :end t)",
		Panics: true,
	}).Test(t)
}

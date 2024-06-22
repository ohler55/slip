// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDeleteIfEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'numberp nil)",
		Expect: "nil",
	}).Test(t)
}

func TestDeleteIfListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(1 2 3 4 5))",
		Expect: "(1 3 5)",
	}).Test(t)
}

func TestDeleteIfListCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(1 2 3 4 5) :count 1)",
		Expect: "(1 3 4 5)",
	}).Test(t)
}

func TestDeleteIfListFromEndCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(1 2 3 4 5) :count 1 :from-end t)",
		Expect: "(1 2 3 5)",
	}).Test(t)
}

func TestDeleteIfListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(1 2 3 4 5) :start 1 :end 3)",
		Expect: "(1 3 4 5)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(1 2 3 4 5) :start 1 :end nil)",
		Expect: "(1 3 5)",
	}).Test(t)
}

func TestDeleteIfListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(1 2 3 4 5 6) :key '1+)",
		Expect: "(2 4 6)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(1 2 3 4 5 6) :key '1+ :from-end t :count 2)",
		Expect: "(1 2 4 6)",
	}).Test(t)
}

func TestDeleteIfVectorPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp #(1 2 3 4 5))",
		Array:  true,
		Expect: "#(1 3 5)",
	}).Test(t)
}

func TestDeleteIfStringPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-if 'evenp "ABCBD" :key 'char-code)`,
		Expect: `"AC"`,
	}).Test(t)
}

func TestDeleteIfStringCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-if 'evenp "ABCBD" :key 'char-code :count 1)`,
		Expect: `"ACBD"`,
	}).Test(t)
}

func TestDeleteIfStringFromEndCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-if 'evenp "ABCBDE" :key 'char-code :count 2 :from-end t)`,
		Expect: `"ABCE"`,
	}).Test(t)
}

func TestDeleteIfStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-if 'evenp "ABCBD" :key 'char-code :start 1 :end 3)`,
		Expect: `"ACBD"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete-if 'evenp "ABCBD" :key 'char-code :start 1 :end nil)`,
		Expect: `"AC"`,
	}).Test(t)
}

func TestDeleteIfNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp t)",
		Panics: true,
	}).Test(t)
}

func TestDeleteIfBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(a b c) :start t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(a b c) :start -1)",
		Panics: true,
	}).Test(t)
}

func TestDeleteIfBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(a b c) :end t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(a b c) :end -1)",
		Panics: true,
	}).Test(t)
}

func TestDeleteIfBadCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(a b c) :count t)",
		Panics: true,
	}).Test(t)
}

func TestDeleteIfBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(a b c) t 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete-if 'evenp '(a b c) :bad 3)",
		Panics: true,
	}).Test(t)
}

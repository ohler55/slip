// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCountIfEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp nil)",
		Expect: "0",
	}).Test(t)
}

func TestCountIfListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3 4 5))",
		Expect: "2",
	}).Test(t)
}

func TestCountIfListFromEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3 4 5) :from-end t)",
		Expect: "2",
	}).Test(t)
}

func TestCountIfListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3 4 5) :start 1 :end 3)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3 4 5) :start 1 :end 3 :from-end t)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3 4 5) :start 1 :end nil)",
		Expect: "2",
	}).Test(t)
}

func TestCountIfListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3 4 5) :key '1+)",
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3 4 5) :key '1+ :from-end t)",
		Expect: "3",
	}).Test(t)
}

func TestCountIfVectorPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp #(1 2 3 4 5))",
		Expect: "2",
	}).Test(t)
}

func TestCountIfStringPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(count-if 'evenp "ABCDE" :key 'char-code)`,
		Expect: "2",
	}).Test(t)
}

func TestCountIfStringFromEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(count-if 'evenp "ABCDE" :key 'char-code :from-end t)`,
		Expect: "2",
	}).Test(t)
}

func TestCountIfStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(count-if 'evenp "ABCDE" :key 'char-code :start 1 :end 3)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(count-if 'evenp "ABCDE" :key 'char-code :from-end t :start 1 :end 3)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(count-if 'evenp "ABCDE" :key 'char-code :start 1 :end nil)`,
		Expect: "2",
	}).Test(t)
}

func TestCountIfNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp t)",
		Panics: true,
	}).Test(t)
}

func TestCountIfBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3) :start t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3) :start -1)",
		Panics: true,
	}).Test(t)
}

func TestCountIfBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3) :end t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3) :end -1)",
		Panics: true,
	}).Test(t)
}

func TestCountIfBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3) t 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3) :bad 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3) :count 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(count-if 'evenp '(1 2 3) :start)",
		Panics: true,
	}).Test(t)
}

func TestCountIfBadPredicate(t *testing.T) {
	(&sliptest.Function{
		Source: "(count-if t '(1 2 3))",
		Panics: true,
	}).Test(t)
}

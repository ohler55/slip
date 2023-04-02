// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStableSortNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestStableSortList(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort '(b a d c))`,
		Expect: "(a b c d)",
	}).Test(t)
}

func TestStableSortStringList(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort '("b" "a" "d" "c"))`,
		Expect: `("a" "b" "c" "d")`,
	}).Test(t)
}

func TestStableSortVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort #(b a d c))`,
		Expect: "#(a b c d)",
	}).Test(t)
}

func TestStableSortString(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort "badc")`,
		Expect: `"abcd"`,
	}).Test(t)
}

func TestStableSortListKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort '(3 1 4 2) nil :key '1+)`,
		Expect: "(1 2 3 4)",
	}).Test(t)
}

func TestStableSortListPredicate(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort '(3 1 4 2) (lambda (x y) (< x y)))`,
		Expect: "(1 2 3 4)",
	}).Test(t)
}

func TestStableSortListPredicateKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort '(3 1 4 2) (lambda (x y) (< x y)) :key '1+)`,
		Expect: "(1 2 3 4)",
	}).Test(t)
}

func TestStableSortNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort t)`,
		Panics: true,
	}).Test(t)
}

func TestStableSortBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(stable-sort '(b a) nil t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(stable-sort '(b a) nil :not-key nil)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSortNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestSortList(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '(b a d c))`,
		Expect: "(a b c d)",
	}).Test(t)
}

func TestSortStringList(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '("b" "a" "d" "c"))`,
		Expect: `("a" "b" "c" "d")`,
	}).Test(t)
}

func TestSortVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort #(b a d c))`,
		Expect: "#(a b c d)",
	}).Test(t)
}

func TestSortString(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort "badc")`,
		Expect: `"abcd"`,
	}).Test(t)
}

func TestSortListKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '(3 1 4 2) nil :key '1+)`,
		Expect: "(1 2 3 4)",
	}).Test(t)
}

func TestSortListPredicate(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '(3 1 4 2) (lambda (x y) (< x y)))`,
		Expect: "(1 2 3 4)",
	}).Test(t)
}

func TestSortListPredicateKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '(3 1 4 2) (lambda (x y) (< x y)) :key '1+)`,
		Expect: "(1 2 3 4)",
	}).Test(t)
}

func TestSortMixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '(#\b a 3 2.0 t nil))`,
		Expect: `(t nil 2 3 a #\b)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(sort '(3 2.0 nil 1.5))`,
		Expect: `(nil 1.5 2 3)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(sort '("b" a "d" c))`,
		Expect: `(a "b" c "d")`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(sort '(#\b 3 #\a nil "x" y "z"))`,
		Expect: `(nil 3 #\a #\b "x" y "z")`,
	}).Test(t)
}

func TestSortMixedCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '(#\b a #\c "d" #\e @2023-04-01T20:24:53Z #\f g #\h))`,
		Expect: `(a #\b #\c "d" #\e #\f g #\h @2023-04-01T20:24:53Z)`,
	}).Test(t)
}

func TestSortMixedString(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '("b" a "c" nil "d" 1 e @2023-04-01T20:24:53Z "f" g "h"))`,
		Expect: `(nil 1 a "b" "c" "d" e "f" g "h" @2023-04-01T20:24:53Z)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(sort '("b" 1 "c" 2 "a" 3))`,
		Expect: `(1 2 3 "a" "b" "c")`,
	}).Test(t)
}

func TestSortMixedSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '(b 1 c 2 a 3))`,
		Expect: `(1 2 3 a b c)`,
	}).Test(t)
}

func TestSortMixedTime(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '((x) @2023-04-01T20:24:57Z @2023-04-01T20:24:53Z))`,
		Expect: `(@2023-04-01T20:24:53Z @2023-04-01T20:24:57Z (x))`,
	}).Test(t)
}

func TestSortMixedList(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '((x) (x y b) (x y)))`,
		Expect: `((x) (x y) (x y b))`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(sort '((x) 1 (x y) @2023-04-01T20:24:53Z (y z) #(1 2) 2))`,
		Expect: `(1 2 @2023-04-01T20:24:53Z (x) (x y) (y z) #(1 2))`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(sort '(3 (x) 1 (x y b) 2 (x y)))`,
		Expect: `(1 2 3 (x) (x y) (x y b))`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(sort '(#(2) (x) (x y)))`,
		Expect: `((x) (x y) #(2))`,
	}).Test(t)
}

func TestSortNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort t)`,
		Panics: true,
	}).Test(t)
}

func TestSortBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(sort '(b a) nil t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(sort '(b a) nil :not-key nil)`,
		Panics: true,
	}).Test(t)
}

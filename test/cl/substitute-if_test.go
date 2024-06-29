// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSubstituteIfListBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a 0 c)))
                  (list (substitute-if 2 'numberp lst) lst))`,
		Expect: "((a 2 c) (a 0 c))",
	}).Test(t)
}

func TestSubstituteIfString(t *testing.T) {
	(&sliptest.Function{
		Source: `(substitute-if #\Q (lambda (v) (equal v #\q)) "quux")`,
		Expect: `"Quux"`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(substitute-if t (lambda (v) nil) "quux")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSubstituteIfVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((vec #(a b c)))
                  (list (substitute-if 2 (lambda (v) (equal v 'b)) vec) vec))`,
		Array:  true,
		Expect: "(#(a 2 c) #(a b c))",
	}).Test(t)
}

func TestSubstituteIfListKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '((1) (2) (3) (4))))
                  (list (substitute-if 0 (lambda (v) (< 3 v)) lst :key 'car) lst))`,
		Expect: "(((1) (2) (3) 0) ((1) (2) (3) (4)))",
	}).Test(t)
}

func TestSubstituteIfListCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a 1 c 2)))
                  (list (substitute-if 0 'numberp lst :from-end t :count 1) lst))`,
		Expect: "((a 1 c 0) (a 1 c 2))",
	}).Test(t)
}

func TestSubstituteIfListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a 1 c 2 d 3)))
                  (list (substitute-if 0 'numberp lst :from-end t :start 2 :end 5) lst))`,
		Expect: "((a 1 c 0 d 3) (a 1 c 2 d 3))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a 1 c 2 d 3)))
                  (list (substitute-if 0 'numberp lst :start 2 :end 5) lst))`,
		Expect: "((a 1 c 0 d 3) (a 1 c 2 d 3))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a 1 c 2 d 3)))
                  (list (substitute-if 0 'numberp lst :start -1 :end 5) lst))`,
		Expect: "((a 0 c 0 d 3) (a 1 c 2 d 3))",
	}).Test(t)
}

func TestSubstituteIfBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(substitute-if 2 'numberp '(1 2) :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSubstituteIfBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(substitute-if 2 'numberp '(1 2) :end t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSubstituteIfBadCount(t *testing.T) {
	(&sliptest.Function{
		Source:    `(substitute-if 2 'numberp '(1 2) :count t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSubstituteIfNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(substitute-if 2 'numberp t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

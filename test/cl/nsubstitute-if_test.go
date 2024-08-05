// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNsubstituteIfListBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a 0 c)))
                  (list (nsubstitute-if 2 'numberp lst) lst))`,
		Expect: "((a 2 c) (a 2 c))",
	}).Test(t)
}

func TestNsubstituteIfString(t *testing.T) {
	(&sliptest.Function{
		Source: `(nsubstitute-if #\Q (lambda (v) (equal v #\q)) "quux")`,
		Expect: `"Quux"`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(nsubstitute-if t (lambda (v) nil) "quux")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestNsubstituteIfOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(nsubstitute-if (coerce #\Q 'octet)
                                 (lambda (v) (equal v (coerce #\q 'octet))) (coerce "quux" 'octets))`,
		Array:  true,
		Expect: "#(81 117 117 120)",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(nsubstitute-if t (lambda (v) nil) (coerce "quux" 'octets))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestNsubstituteIfVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((vec #(a b c)))
                  (list (nsubstitute-if 2 (lambda (v) (equal v 'b)) vec) vec))`,
		Array:  true,
		Expect: "(#(a 2 c) #(a 2 c))",
	}).Test(t)
}

func TestNsubstituteIfListKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '((1) (2) (3) (4))))
                  (list (nsubstitute-if 0 (lambda (v) (< 3 v)) lst :key 'car) lst))`,
		Expect: "(((1) (2) (3) 0) ((1) (2) (3) 0))",
	}).Test(t)
}

func TestNsubstituteIfListCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a 1 c 2)))
                  (list (nsubstitute-if 0 'numberp lst :from-end t :count 1) lst))`,
		Expect: "((a 1 c 0) (a 1 c 0))",
	}).Test(t)
}

func TestNsubstituteIfListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a 1 c 2 d 3)))
                  (list (nsubstitute-if 0 'numberp lst :from-end t :start 2 :end 5) lst))`,
		Expect: "((a 1 c 0 d 3) (a 1 c 0 d 3))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a 1 c 2 d 3)))
                  (list (nsubstitute-if 0 'numberp lst :start 2 :end 5) lst))`,
		Expect: "((a 1 c 0 d 3) (a 1 c 0 d 3))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a 1 c 2 d 3)))
                  (list (nsubstitute-if 0 'numberp lst :start -1 :end 5) lst))`,
		Expect: "((a 0 c 0 d 3) (a 0 c 0 d 3))",
	}).Test(t)
}

func TestNsubstituteIfBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nsubstitute-if 2 'numberp '(1 2) :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestNsubstituteIfBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nsubstitute-if 2 'numberp '(1 2) :end t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestNsubstituteIfBadCount(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nsubstitute-if 2 'numberp '(1 2) :count t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestNsubstituteIfNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nsubstitute-if 2 'numberp t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

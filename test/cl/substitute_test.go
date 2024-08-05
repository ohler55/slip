// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSubstituteListBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a b c)))
                  (list (substitute 2 'b lst) lst))`,
		Expect: "((a 2 c) (a b c))",
	}).Test(t)
}

func TestSubstituteString(t *testing.T) {
	(&sliptest.Function{
		Source: `(substitute #\Q #\q "quux")`,
		Expect: `"Quux"`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(substitute t #\q "quux")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSubstituteOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(substitute (coerce #\Q 'octet) (coerce #\q 'octet) (coerce "quux" 'octets))`,
		Array:  true,
		Expect: "#(81 117 117 120)",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(substitute t (coerce #\q 'octet) (coerce "quux" 'octets))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSubstituteVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((vec #(a b c)))
                  (list (substitute 2 'b vec) vec))`,
		Array:  true,
		Expect: "(#(a 2 c) #(a b c))",
	}).Test(t)
}

func TestSubstituteListKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '((1) (2) (3) (4))))
                  (list (substitute 0 3 lst :key 'car :test '<) lst))`,
		Expect: "(((1) (2) (3) 0) ((1) (2) (3) (4)))",
	}).Test(t)
}

func TestSubstituteOctetsKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcd" 'octets)))
                  (list (substitute (coerce #\x 'octet) 98 seq :key (lambda (x) (coerce x 'fixnum)) :test '<) seq))`,
		Array:  true,
		Expect: "(#(97 98 120 120) #(97 98 99 100))",
	}).Test(t)
}

func TestSubstituteListCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a b c b)))
                  (list (substitute 2 'b lst :from-end t :count 1) lst))`,
		Expect: "((a b c 2) (a b c b))",
	}).Test(t)
}

func TestSubstituteOctetsCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcb" 'octets)))
                  (list (substitute (coerce #\x 'octet) (coerce #\b 'octet) seq :from-end t :count 1) seq))`,
		Array:  true,
		Expect: "(#(97 98 99 120) #(97 98 99 98))",
	}).Test(t)
}

func TestSubstituteListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a b c b d b)))
                  (list (substitute 2 'b lst :from-end t :start 2 :end 5) lst))`,
		Expect: "((a b c 2 d b) (a b c b d b))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a b c b d b)))
                  (list (substitute 2 'b lst :start 2 :end 5) lst))`,
		Expect: "((a b c 2 d b) (a b c b d b))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a b c b d b)))
                  (list (substitute 2 'b lst :start -1 :end 5) lst))`,
		Expect: "((a 2 c 2 d b) (a b c b d b))",
	}).Test(t)
}

func TestSubstituteBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(substitute 2 1 '(1 2) :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSubstituteBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(substitute 2 1 '(1 2) :end t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSubstituteBadCount(t *testing.T) {
	(&sliptest.Function{
		Source:    `(substitute 2 1 '(1 2) :count t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSubstituteNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(substitute 2 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

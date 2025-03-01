// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMismatchEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch nil nil)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '() '())",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '() '(a))",
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(a) '())",
		Expect: "0",
	}).Test(t)
}

func TestMismatchBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(1 2 3))",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(1 0 3))",
		Expect: "1",
	}).Test(t)
}

func TestMismatchShort(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(1 2 3 4))",
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3 4) '(1 2 3))",
		Expect: "3",
	}).Test(t)
}

func TestMismatchFromEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(1 2 3) :from-end t)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(2 1 3) :from-end nil)",
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(2 1 3) :from-end t)",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(0 2 1 3) :from-end t)",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3) '(2 1 3) :from-end t)",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3) '(1 2 3) :from-end t)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(0 1 2 3) :from-end t)",
		Expect: "0",
	}).Test(t)
}

func TestMismatchKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '((a . 1) (b . 2) (c . 3)) '((x . 1) (y . 2) (z . 3)) :key 'cdr)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '((a . 1) (b . 2) (c . 3)) '((x . 1) (y . 0) (z . 3)) :key 'cdr)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '((a . 1) (b . 2) (c . 3)) '((x . 1) (y . 2) (z . 3)) :key 'cdr :from-end t)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '((a . 1) (b . 2) (c . 3)) '((x . 1) (y . 0) (z . 3)) :key 'cdr :from-end t)",
		Expect: "2",
	}).Test(t)
}

func TestMismatchTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(2 3 4) :test '<)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(2 2 4) :test '<)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(2 3 4) :test '< :from-end t)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(2 2 4) :test '< :from-end t)",
		Expect: "2",
	}).Test(t)
}

func TestMismatchStart1(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3) '(1 2 3) :start1 1 :end1 nil)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3) '(1 0 3) :start1 1)",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3) '(1 2 3) :start1 1 :from-end t)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3) '(1 0 3) :start1 1 :from-end t)",
		Expect: "3",
	}).Test(t)
}

func TestMismatchStartEnd1(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3 4) '(1 2 3) :start1 1 :end1 4)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3 4) '(1 0 3) :start1 1 :end1 4)",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3 4) '(1 2 3) :start1 1 :end1 4 :from-end t)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(0 1 2 3 4) '(1 0 3) :start1 1 :end1 4 :from-end t)",
		Expect: "3",
	}).Test(t)
}

func TestMismatchStart2(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(0 1 2 3) :start2 1 :end2 nil)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 0 3) '(0 1 2 3) :start2 1)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(0 1 2 3) :start2 1 :from-end t)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 0 3) '(0 1 2 3) :start2 1 :from-end t)",
		Expect: "2",
	}).Test(t)
}

func TestMismatchStartEnd2(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(0 1 2 3 4) :start2 1 :end2 4)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 0 3) '(0 1 2 3 4) :start2 1 :end2 4)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) '(0 1 2 3 4) :start2 1 :end2 4 :from-end t)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 0 3) '(0 1 2 3 4) :start2 1 :end2 4 :from-end t)",
		Expect: "2",
	}).Test(t)
}

func TestMismatchVector(t *testing.T) {
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) #(1 2 3))",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch '(1 2 3) #(1 0 3))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch #(1 2 3) '(1 2 3))",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch #(1 2 3) '(1 0 3))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(mismatch #(1 2 3) #(1 0 3))",
		Expect: "1",
	}).Test(t)
}

func TestMismatchString(t *testing.T) {
	(&sliptest.Function{
		Source: `(mismatch '(#\a #\b #\c) "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mismatch '(#\a #\b #\c) "axc")`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mismatch '(#\a #\x #\c) "abc")`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mismatch "abc" "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mismatch "axc" "abc")`,
		Expect: "1",
	}).Test(t)
}

func TestMismatchOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(mismatch (string-to-octets "abc") (string-to-octets "abc"))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mismatch (string-to-octets "abc") (string-to-octets "axc"))`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mismatch (string-to-octets "abc") '(97 98 99))`,
		Expect: "nil",
	}).Test(t)
}

func TestMismatchNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    "(mismatch 7 '(1))",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(mismatch '(1) 7)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMismatchBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :empty)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :bad t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) t t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMismatchBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :start1 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :start2 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMismatchBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :end1 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :end2 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMismatchHighStart(t *testing.T) {
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :start1 2)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :start2 2)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestMismatchHighEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :end1 2)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(mismatch '(1) '(1) :end2 2)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestMismatchEndBeforeStart(t *testing.T) {
	(&sliptest.Function{
		Source:    "(mismatch '(1 2 3) '(1 2 3) :start1 2 :end1 1)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(mismatch '(1 2 3) '(1 2 3) :start2 2 :end2 1)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

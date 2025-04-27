// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReplaceEmptyList(t *testing.T) {
	(&sliptest.Function{
		Source: "(replace nil nil)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(replace '() '())",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(replace '() '(a))",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(replace '(a) '())",
		Expect: "(a)",
	}).Test(t)
}

func TestReplaceListList(t *testing.T) {
	(&sliptest.Function{
		Source: "(replace '(1 2 3 4 5) '(5 4 3 2 1) :start1 2 :end1 4 :start2 1 :end2 5)",
		Expect: "(1 2 4 3 5)",
	}).Test(t)
}

func TestReplaceListVector(t *testing.T) {
	(&sliptest.Function{
		Source: "(replace '(1 2 3 4 5) #(5 4 3 2 1) :start1 2 :end1 4 :start2 1 :end2 5)",
		Expect: "(1 2 4 3 5)",
	}).Test(t)
}

func TestReplaceListString(t *testing.T) {
	(&sliptest.Function{
		Source: `(replace '(1 2 3 4 5) "abcde" :start1 2 :end1 4 :start2 1 :end2 5)`,
		Expect: `(1 2 #\b #\c 5)`,
	}).Test(t)
}

func TestReplaceListOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(replace '(1 2 3 4 5) (coerce '(5 4 3 2 1) 'octets) :start1 2 :end1 4 :start2 1 :end2 5)`,
		Expect: `(1 2 4 3 5)`,
	}).Test(t)
}

func TestReplaceVectorList(t *testing.T) {
	(&sliptest.Function{
		Source: "(replace #(1 2 3 4 5) '(5 4 3 2 1) :start1 2 :end1 nil :start2 1 :end2 nil)",
		Array:  true,
		Expect: "#(1 2 4 3 2)",
	}).Test(t)
}

func TestReplaceOctetsList(t *testing.T) {
	(&sliptest.Function{
		Source: `(replace (coerce '(1 2 3 4 5) 'octets) '(5 4 3 2 1) :start1 2 :end1 4 :start2 1 :end2 5)`,
		Array:  true,
		Expect: `#(1 2 4 3 5)`,
	}).Test(t)
}

func TestReplaceBitVectorList(t *testing.T) {
	(&sliptest.Function{
		Source: "(replace #*10100101 #*0011 :start1 3 :end1 nil :start2 1 :end2 nil)",
		Expect: "#*10101101",
	}).Test(t)
	(&sliptest.Function{
		Source: "(replace #*10100101 #*001100 :start1 4 :end1 nil :start2 0 :end2 nil)",
		Expect: "#*10100011",
	}).Test(t)
}

func TestReplaceStringList(t *testing.T) {
	(&sliptest.Function{
		Source: `(replace "abcde" '(#\w #\x #\y #\z #\X) :start1 2 :end1 nil :start2 1 :end2 nil)`,
		Expect: `"abxyz"`,
	}).Test(t)
}

func TestReplaceStringNotChar(t *testing.T) {
	(&sliptest.Function{
		Source:    `(replace "abcde" '(1 2))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplaceNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    "(replace 7 '(1))",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(replace '(1) 7)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplaceBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    "(replace '(1) '(1) :empty)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(replace '(1) '(1) :bad t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(replace '(1) '(1) t t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplaceBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    "(replace '(1) '(1) :start1 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(replace '(1) '(1) :start2 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplaceBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    "(replace '(1) '(1) :end1 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(replace '(1) '(1) :end2 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplaceHighStart(t *testing.T) {
	(&sliptest.Function{
		Source:    "(replace '(1) '(1) :start1 2)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestReplaceHighEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    "(replace '(1) '(1) :end1 2)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestReplaceEndBeforeStart(t *testing.T) {
	(&sliptest.Function{
		Source:    "(replace '(1 2 3) '(1 2 3) :start1 2 :end1 1)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

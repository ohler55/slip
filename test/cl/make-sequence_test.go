// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeSequenceListBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-sequence 'list 3)`,
		Expect: "(nil nil nil)",
	}).Test(t)
}

func TestMakeSequenceListInitialElement(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-sequence 'list 3 :initial-element 'slip)`,
		Expect: "(slip slip slip)",
	}).Test(t)
}

func TestMakeSequenceOctetsBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-sequence 'octets 3)`,
		Array:  true,
		Expect: "#(0 0 0)",
	}).Test(t)
}

func TestMakeSequenceOctetstInitialElement(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-sequence 'octets 3 :initial-element #\x)`,
		Array:  true,
		Expect: "#(120 120 120)",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-sequence 'octets 3 :initial-element t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestMakeSequenceStringBasic(t *testing.T) {
	(&sliptest.Function{
		Source:   `(make-sequence 'string 3)`,
		Readably: true,
		Expect:   `"\u0000\u0000\u0000"`,
	}).Test(t)
}

func TestMakeSequenceStringInitialElement(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-sequence 'string 3 :initial-element #\.)`,
		Expect: `"..."`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-sequence 'string 3 :initial-element t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeVectorListBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-sequence 'vector 3)`,
		Array:  true,
		Expect: "#(nil nil nil)",
	}).Test(t)
}

func TestMakeSequenceVectorInitialElement(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-sequence '(vector fixnum) 3 :initial-element 7)`,
		Array:  true,
		Expect: "#(7 7 7)",
	}).Test(t)
}

func TestMakeSequenceBitVectorBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-sequence 'bit-vector 4)`,
		Expect: "#*0000",
	}).Test(t)
}

func TestMakeSequenceBitVectorInitialElement(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-sequence 'bit-vector 4 :initial-element 1)`,
		Expect: "#*1111",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-sequence 'bit-vector 3 :initial-element t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeSequenceBadSize(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-sequence 'list t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeSequenceBadResultType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-sequence 'fixnum 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-sequence t 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeSequenceVectorBadElementType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-sequence '(vector t) 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-sequence '(vector t t) 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

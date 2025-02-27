// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEltListOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(elt '(a b c) 1)`,
		Expect: "b",
	}).Test(t)
}

func TestEltListSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq '(a b c))) (setf (elt seq 1) 'x) seq)`,
		Expect: "(a x c)",
	}).Test(t)
}

func TestEltVectorOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(elt #(a b c) 1)`,
		Expect: "b",
	}).Test(t)
}

func TestEltVectorSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq #(a b c))) (setf (elt seq 1) 'x) seq)`,
		Array:  true,
		Expect: "#(a x c)",
	}).Test(t)
}

func TestEltStringOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(elt "abc" 1)`,
		Expect: `#\b`,
	}).Test(t)
}

func TestEltStringSetf(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (elt "abc" 1) #\x)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestEltOctetsOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(elt (coerce "abc" 'octets) 1)`,
		Expect: `98`,
	}).Test(t)
}

func TestEltOctetsSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abc" 'octets))) (setf (elt seq 1) 120) seq)`,
		Array:  true,
		Expect: `#(97 120 99)`,
	}).Test(t)
}

func TestEltBadSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(elt t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestEltBadIndex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(elt '(a b c) -1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(elt '(a b c) 3)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestEltNotSequenceSetf(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (elt t 1) 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

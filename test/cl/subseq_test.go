// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSubseqString(t *testing.T) {
	(&sliptest.Function{
		Source: `(subseq "abcd" 2)`,
		Expect: `"cd"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(subseq "abcd" 1 3)`,
		Expect: `"bc"`,
	}).Test(t)
}

func TestSubseqList(t *testing.T) {
	(&sliptest.Function{
		Source: `(subseq '(a b c d) 2)`,
		Expect: "(c d)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(subseq '(a b c d) 1 3)`,
		Expect: "(b c)",
	}).Test(t)
}

func TestSubseqVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(subseq #(a b c d) 2)`,
		Expect: "#(c d)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(subseq #(a b c d) 1 3)`,
		Expect: "#(b c)",
	}).Test(t)
}

func TestSubseqListSetf(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("seq"),
		slip.List{
			slip.Symbol("a"),
			slip.Symbol("b"),
			slip.Symbol("c"),
			slip.Symbol("d"),
		})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setf (subseq seq 2) '(x y z))`,
		Expect: "(x y z)",
	}).Test(t)
	tt.Equal(t, "(a b x y)", slip.ObjectString(scope.Get(slip.Symbol("seq"))))
}

func TestSubseqVectorSetf(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("seq"),
		slip.NewVector(slip.List{
			slip.Symbol("a"),
			slip.Symbol("b"),
			slip.Symbol("c"),
			slip.Symbol("d"),
		}, slip.TrueSymbol, true))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setf (subseq seq 2) #(x y z))`,
		Expect: "#(x y z)",
	}).Test(t)
	tt.Equal(t, "#(a b x y)", slip.ObjectString(scope.Get(slip.Symbol("seq"))))
}

func TestSubseqStringSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(setf (subseq "abcd" 2) "xyz")`,
		Panics: true,
	}).Test(t)
}

func TestSubseqListSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(setf (subseq '(a b c d) 2) t)`,
		Panics: true,
	}).Test(t)
}

func TestSubseqVectorSetfNotVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(setf (subseq #(a b c d) 2) t)`,
		Panics: true,
	}).Test(t)
}

func TestSubseqBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: `(subseq '(a b c d) t)`,
		Panics: true,
	}).Test(t)
}

func TestSubseqNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: `(subseq t 2)`,
		Panics: true,
	}).Test(t)
}

func TestSubseqBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(subseq '(a b c d) 1 t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(subseq '(a b c d) 1 -1)`,
		Panics: true,
	}).Test(t)
}

func TestSubseqOutOfBounds(t *testing.T) {
	(&sliptest.Function{
		Source: `(subseq '(a b c d) 5)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(subseq "abcd" 5)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(subseq #(a b c d) 5)`,
		Panics: true,
	}).Test(t)
}

// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFillListAll(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((seq '(a b c d))
                        (seq2 (fill seq 'x)))
                  (list seq seq2))`,
		Expect: "((x x x x) (x x x x))",
	}).Test(t)
}

func TestFillListRange(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((seq '(a b c d))
                        (seq2 (fill seq 'x :start 1 :end 3)))
                  (list seq seq2))`,
		Expect: "((a x x d) (a x x d))",
	}).Test(t)
}

func TestFillStringAll(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((seq "abcd")
                        (seq2 (fill seq #\x)))
                  (list seq seq2))`,
		Expect: `("abcd" "xxxx")`,
	}).Test(t)
}

func TestFillStringRange(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((seq "abcd")
                        (seq2 (fill seq #\x :start 1 :end 3)))
                  (list seq seq2))`,
		Expect: `("abcd" "axxd")`,
	}).Test(t)
}

func TestFillVectorAll(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((seq #(a b c d))
                        (seq2 (fill seq 'x)))
                  (list seq seq2))`,
		Array:  true,
		Expect: "(#(x x x x) #(x x x x))",
	}).Test(t)
}

func TestFillVectorRange(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((seq #(a b c d))
                        (seq2 (fill seq 'x :start 1 :end 3)))
                  (list seq seq2))`,
		Array:  true,
		Expect: "(#(a x x d) #(a x x d))",
	}).Test(t)
}

func TestFillOctetsAll(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((seq (coerce "abcd" 'octets))
                        (seq2 (fill seq 66)))
                  (list seq seq2))`,
		Array:  true,
		Expect: `(#(66 66 66 66) #(66 66 66 66))`,
	}).Test(t)
}

func TestFillOctetsRange(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((seq (coerce "abcd" 'octets))
                        (seq2 (fill seq 66 :start 1 :end 3)))
                  (list seq seq2))`,
		Array:  true,
		Expect: `(#(97 66 66 100) #(97 66 66 100))`,
	}).Test(t)
}

func TestFillBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fill '(a b c) 'x :start 3)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestFillBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fill '(a b c) 'x :end 3)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestFillEndBeforeStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fill '(a b c) 'x :start 2 :end 1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestFillStringNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fill "abc" 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestFillNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fill t 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

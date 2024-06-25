// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCopySeqNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(copy-seq nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestCopySeqList(t *testing.T) {
	(&sliptest.Function{
		Source: `(copy-seq '(a b c))`,
		Expect: "(a b c)",
	}).Test(t)
}

func TestCopySeqString(t *testing.T) {
	(&sliptest.Function{
		Source: `(copy-seq "abc")`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestCopySeqVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(copy-seq #(a b c))`,
		Array:  true,
		Expect: "#(a b c)",
	}).Test(t)
}

func TestCopySeqEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a b c)))
                  (list (equal lst (copy-seq lst))
                        (eq lst (copy-seq lst))))`,
		Expect: "(t nil)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((seq "abc"))
                  (list (equal seq (copy-seq seq))
                        (eq seq (copy-seq seq))))`,
		Expect: "(t nil)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst #(a b c)))
                  (list (equal lst (copy-seq lst))
                        (eq lst (copy-seq lst))))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestCopySeqNotSeq(t *testing.T) {
	(&sliptest.Function{
		Source:    `(copy-seq t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

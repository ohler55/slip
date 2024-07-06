// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCopyListContent(t *testing.T) {
	(&sliptest.Function{
		Source: `(copy-list '(a b c))`,
		Expect: "(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(copy-list nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestCopyListEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a b c)))
                  (list (equal lst (copy-list lst)) (eq lst (copy-list lst))))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestCopyListNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(copy-list t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCopyAlistContent(t *testing.T) {
	(&sliptest.Function{
		Source: `(copy-alist '(a (b . 2) (c . 3)))`,
		Expect: "(a (b . 2) (c . 3))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(copy-alist nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestCopyAlistEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a (b . 2) (c . 3))))
                  (list (equal lst (copy-alist lst))
                        (eq lst (copy-alist lst))
                        (eq (car lst) (car (copy-alist lst)))
                        (eq (cadr lst) (cadr (copy-alist lst)))))`,
		Expect: "(t nil t nil)",
	}).Test(t)
}

func TestCopyAlistNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(copy-alist t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

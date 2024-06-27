// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCopyTreeContent(t *testing.T) {
	(&sliptest.Function{
		Source: `(copy-tree '(a (b . 2) (c 3)))`,
		Expect: "(a (b . 2) (c 3))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(copy-tree nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestCopyTreeEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a (b . 2) (c 3))))
                  (list (equal lst (copy-tree lst))
                        (eq lst (copy-tree lst))
                        (eq (car lst) (car (copy-tree lst)))
                        (eq (cadr lst) (cadr (copy-tree lst)))))`,
		Expect: "(t nil t nil)",
	}).Test(t)
}

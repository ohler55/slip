// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSublisBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a (b (c)))))
                  (list (sublis '((a . 1) (b . 2) (c . 3)) lst) lst))`,
		Expect: "((1 (2 (3))) (a (b (c))))",
	}).Test(t)
}

func TestSublisKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(1 2 (3 . 4))))
                  (list (sublis '((4 . 99)) lst :key (lambda (x) (if (numberp x) (* x x) 0)) :test '<) lst))`,
		Expect: "((1 2 (99 . 99)) (1 2 (3 . 4)))",
	}).Test(t)
}

func TestSublisNotAssoc(t *testing.T) {
	(&sliptest.Function{
		Source:    `(sublis '(t) '(a b))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSublisNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(sublis t '(a b))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

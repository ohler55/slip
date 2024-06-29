// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNsublisBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a (b (c)))))
                  (list (nsublis '((a . 1) (b . 2) (c . 3)) lst) lst))`,
		Expect: "((1 (2 (3))) (1 (2 (3))))",
	}).Test(t)
}

func TestNsublisKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(1 2 (3 . 4))))
                  (list (nsublis '((4 . 99)) lst :key (lambda (x) (if (numberp x) (* x x) 0)) :test '<) lst))`,
		Expect: "((1 2 (99 . 99)) (1 2 (99 . 99)))",
	}).Test(t)
}

func TestNsublisNotAssoc(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nsublis '(t) '(a b))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestNsublisNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nsublis t '(a b))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

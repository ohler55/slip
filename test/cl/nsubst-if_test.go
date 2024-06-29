// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNsubstIfBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a (b (c)))))
                  (list (nsubst-if 2 (lambda (v) (equal v 'b)) lst) lst))`,
		Expect: "((a (2 (c))) (a (2 (c))))",
	}).Test(t)
}

func TestNsubstIfKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(1 2 (3 . 4))))
                  (list (nsubst-if 99 (lambda (v) (< 4 v)) lst :key (lambda (x) (if (numberp x) (* x x) 0))) lst))`,
		Expect: "((1 2 (99 . 99)) (1 2 (99 . 99)))",
	}).Test(t)
}

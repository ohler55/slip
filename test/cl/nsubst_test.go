// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNsubstBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a (b (c)))))
                  (list (nsubst 2 'b lst) lst))`,
		Expect: "((a (2 (c))) (a (2 (c))))",
	}).Test(t)
}

func TestNsubstKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(1 2 (3 . 4))))
                  (list (nsubst 99 4 lst :key (lambda (x) (if (numberp x) (* x x) 0)) :test '<) lst))`,
		Expect: "((1 2 (99 . 99)) (1 2 (99 . 99)))",
	}).Test(t)
}

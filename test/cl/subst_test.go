// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSubstBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a (b (c)))))
                  (list (subst 2 'b lst) lst))`,
		Expect: "((a (2 (c))) (a (b (c))))",
	}).Test(t)
}

func TestSubstKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(1 2 (3 . 4))))
                  (list (subst 99 4 lst :key (lambda (x) (if (numberp x) (* x x) 0)) :test '<) lst))`,
		Expect: "((1 2 (99 . 99)) (1 2 (3 . 4)))",
	}).Test(t)
}

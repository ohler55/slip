// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPushSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(b c)))
                  (list (push 'a lst) lst))`,
		Expect: "((a b c) (a b c))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst nil))
                  (list (push 'a lst) lst))`,
		Expect: "((a) (a))",
	}).Test(t)
}

func TestPushPlacer(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '((b c))))
                  (list (push 'a (car lst)) lst))`,
		Expect: "((a b c) ((a b c)))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a ((b c)) d)))
                  (list (push 'a (car (nth 1 lst))) lst))`,
		Expect: "((a b c) (a ((a b c)) d))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a (nil) d)))
                  (list (push 'b (car (nth 1 lst))) lst))`,
		Expect: "((b) (a ((b)) d))",
	}).Test(t)
}

func TestPushNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(let ((lst t)) (push 1 lst))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(let ((lst '(t))) (push 1 (car lst)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestPushNotPlacer(t *testing.T) {
	(&sliptest.Function{
		Source:    `(push 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

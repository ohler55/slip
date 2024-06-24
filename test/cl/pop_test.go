// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPopSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a b c)))
                  (list (pop lst) lst))`,
		Expect: "(a (b c))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst nil))
                  (list (pop lst) lst))`,
		Expect: "(nil nil)",
	}).Test(t)
}

func TestPopPlacer(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '((a b c))))
                  (list (pop (car lst)) lst))`,
		Expect: "(a ((b c)))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a ((b c)) d)))
                  (list (pop (car (nth 1 lst))) lst))`,
		Expect: "(b (a ((c)) d))",
	}).Test(t)
}

func TestPopNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(let ((lst t)) (pop lst))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(let ((lst '(t))) (pop (car lst)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestPopNotPlacer(t *testing.T) {
	(&sliptest.Function{
		Source:    `(pop t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestPopFoo(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a ((b c)) d)))
                  (list (pop (car (nth 1 lst))) lst))`,
		Expect: "(b (a ((c)) d))",
	}).Test(t)
}

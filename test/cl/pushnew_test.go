// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPushnewSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(b c)))
                  (pushnew 'a lst)
                  (list (pushnew 'a lst) lst))`,
		Expect: "((a b c) (a b c))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst nil))
                  (list (pushnew 'a lst) lst))`,
		Expect: "((a) (a))",
	}).Test(t)
}

func TestPushnewPlacer(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '((b c))))
                  (list (pushnew 'a (car lst)) (pushnew 'a (car lst)) lst))`,
		Expect: "((a b c) (a b c) ((a b c)))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a ((b c)) d)))
                  (list (pushnew 'a (car (nth 1 lst))) lst))`,
		Expect: "((a b c) (a ((a b c)) d))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a (nil) d)))
                  (list (pushnew 'b (car (nth 1 lst))) lst))`,
		Expect: "((b) (a ((b)) d))",
	}).Test(t)
}

func TestPushnewSymbolKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '((b . 1) (c . 2))))
                  (pushnew '(a . 0) lst :key 'cdr :test '>=)
                  (list (pushnew '(a . 0) lst :key 'cdr :test '>=) lst))`,
		Expect: "(((a . 0) (b . 1) (c . 2)) ((a . 0) (b . 1) (c . 2)))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '((b . 1) (c . 2))))
                  (pushnew '(a . 0) lst :key 'cdr :test-not '<)
                  (list (pushnew '(a . 0) lst :key 'cdr :test-not '<) lst))`,
		Expect: "(((a . 0) (b . 1) (c . 2)) ((a . 0) (b . 1) (c . 2)))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '((b . 1) (c . 2))))
                  (pushnew '(a . 0) lst :key 'cdr)
                  (list (pushnew '(a . 0) lst :key 'cdr) lst))`,
		Expect: "(((a . 0) (b . 1) (c . 2)) ((a . 0) (b . 1) (c . 2)))",
	}).Test(t)
}

func TestPushnewNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(let ((lst t)) (pushnew 1 lst))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(let ((lst '(t))) (pushnew 1 (car lst)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestPushnewNotPlacer(t *testing.T) {
	(&sliptest.Function{
		Source:    `(pushnew 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAddnewSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(b c)))
                  (addnew 'a lst)
                  (list (addnew 'a lst) lst))`,
		Expect: "((b c a) (b c a))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst nil))
                  (list (addnew 'a lst) lst))`,
		Expect: "((a) (a))",
	}).Test(t)
}

func TestAddnewPlacer(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '((b c))))
                  (list (addnew 'a (car lst)) (addnew 'a (car lst)) lst))`,
		Expect: "((b c a) (b c a) ((b c a)))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a ((b c)) d)))
                  (list (addnew 'a (car (nth 1 lst))) lst))`,
		Expect: "((b c a) (a ((b c a)) d))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '(a (nil) d)))
                  (list (addnew 'b (car (nth 1 lst))) lst))`,
		Expect: "((b) (a ((b)) d))",
	}).Test(t)
}

func TestAddnewSymbolKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '((b . 1) (c . 2))))
                  (addnew '(a . 0) lst :key 'cdr :test '>=)
                  (list (addnew '(a . 0) lst :key 'cdr :test '>=) lst))`,
		Expect: "(((b . 1) (c . 2) (a . 0)) ((b . 1) (c . 2) (a . 0)))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '((b . 1) (c . 2))))
                  (addnew '(a . 0) lst :key 'cdr :test-not '<)
                  (list (addnew '(a . 0) lst :key 'cdr :test-not '<) lst))`,
		Expect: "(((b . 1) (c . 2) (a . 0)) ((b . 1) (c . 2) (a . 0)))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((lst '((b . 1) (c . 2))))
                  (addnew '(a . 0) lst :key 'cdr)
                  (list (addnew '(a . 0) lst :key 'cdr) lst))`,
		Expect: "(((b . 1) (c . 2) (a . 0)) ((b . 1) (c . 2) (a . 0)))",
	}).Test(t)
}

func TestAddnewNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(let ((lst t)) (addnew 1 lst))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(let ((lst '(t))) (addnew 1 (car lst)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestAddnewNotPlacer(t *testing.T) {
	(&sliptest.Function{
		Source:    `(addnew 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

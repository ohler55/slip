// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFunBasic(t *testing.T) {
	scope := slip.NewScope()
	defer func() {
		slip.ReadString("(makunbound 'quux)", scope).Eval(scope, nil)
	}()
	_ = slip.ReadString(`
(defun quux (a b)
  (* (+ a b) (- a b)))
`, scope).Eval(scope, nil)

	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60)) (pretty-print quux nil))`,
		Expect: `"(defun quux (a b)
  (* (+ a b) (- a b)))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 18)) (pretty-print quux nil))`,
		Expect: `"(defun quux (a b)
  (* (+ a b)
     (- a b)))
"`,
	}).Test(t)

}

func TestFunDotimes(t *testing.T) {
	scope := slip.NewScope()
	defer func() {
		slip.ReadString("(makunbound 'quux)", scope).Eval(scope, nil)
	}()
	_ = slip.ReadString(`
(defun quux (a b)
  "quack quack"
  (dotimes (x 3)
    (* (+ a b x) (- a b x))))
`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print quux nil))`,
		Expect: `"(defun quux (a b)
  "quack quack"
  (dotimes (x 3)
    (* (+ a b x)
       (- a b x))))
"`,
	}).Test(t)
}

func TestFunBuiltin(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60)) (pretty-print cadr nil))`,
		Expect: `"(defun cadr (arg)
  "cadr returns (car (cdr arg))."
  ...)
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60)) (pretty-print common-lisp:cadr nil))`,
		Expect: `"(defun cadr (arg)
  "cadr returns (car (cdr arg))."
  ...)
"`,
	}).Test(t)
}

func TestFunMacro(t *testing.T) {
	scope := slip.NewScope()
	defer func() {
		slip.ReadString("(makunbound 'quux)", scope).Eval(scope, nil)
	}()
	_ = slip.ReadString(`
(defmacro quux (a b)
  (* (+ a b) (- a b)))
`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60)) (pretty-print quux nil))`,
		Expect: `"(defmacro quux (a b)
  (* (+ a b) (- a b)))
"`,
	}).Test(t)
}

func TestFunTight(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20))
                   (pretty-print (dolist (x '(aaa bbb ccc))
                                   (+ x 5)) nil))`,
		Expect: `"(dolist
    (x '(aaa bbb ccc))
  (+ x 5))
"`,
	}).Test(t)
}

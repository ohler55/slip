// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAppendList(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print (1 2 3 4) nil))`,
		Expect: `"(1 2 3 4)
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 5)) (pretty-print (1 2 3 4) nil))`,
		Expect: `"(1 2
 3 4)
"`,
	}).Test(t)
}

func TestAppendVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 6)) (pretty-print #(a b c d) nil))`,
		Expect: `"#(a b
  c d)
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print #(a b c d) nil))`,
		Expect: `"#(a b c d)
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print #() nil))`,
		Expect: `"#()
"`,
	}).Test(t)
}

func TestAppendArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 10)) (pretty-print #2A((a b) (c d)) nil))`,
		Expect: `"#2A((a b)
    (c d))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print #0A() nil))`,
		Expect: `"#0A()
"`,
	}).Test(t)
}

func TestAppendOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)
                       (oo (coerce #(96 97 98) 'octets)))
                   (pretty-print oo nil))`,
		Expect: `"#(96 97 98)
"`,
	}).Test(t)
}

func TestAppendLet(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print (let ((a 1)(b 2)) (+ a b)) nil))`,
		Expect: `"(let ((a 1)
      (b 2))
  (+ a b))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 16))
                   (pretty-print (let ((aaa 1)(bbb 2)) (* (+ aaa bbb) (- aaa bbb))) nil))`,
		Expect: `"(let ((aaa 1)
      (bbb 2))
  (*
   (+ aaa bbb)
   (- aaa bbb)))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print (let (a (b 2)) (+ a b)) nil))`,
		Expect: `"(let (a
      (b 2))
  (+ a b))
"`,
	}).Test(t)
}

func TestAppendDefvar(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print (defvar x) nil))`,
		Expect: `"(defvar x)
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print (defvar x 71) nil))`,
		Expect: `"(defvar x 71)
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print (defvar x 71 "with some documentation") nil))`,
		Expect: `"(defvar x 71
  "with some
   documentation")
"`,
	}).Test(t)
}

func TestAppendDefun(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print (defun quux (a b) (+ a b)) nil))`,
		Expect: `"(defun quux (a b)
  (+ a b))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 14)) (pretty-print (defun quux (a b) (+ a b)) nil))`,
		Expect: `"(defun quux (a
             b)
  (+ a b))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20))
  (pretty-print (defun quux (a b) "quux is a quacker" (* (+ a b) (- a b))) nil))`,
		Expect: `"(defun quux (a b)
  "quux is a quacker"
  (* (+ a b) (- a b)))
"`,
	}).Test(t)
}

func TestAppendQuote(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20)) (pretty-print '(a 2 b 3) nil))`,
		Expect: `"'(a 2 b 3)
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 15)) (pretty-print (list x '(a 2 b 3)) nil))`,
		Expect: `"(list
 x '(a 2 b 3))
"`,
	}).Test(t)
}

func TestAppendLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 15)) (pretty-print (lambda () (princ 'hi)) nil))`,
		Expect: `"(lambda ()
  (princ 'hi))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30)) (pretty-print (lambda (&optional (a 1) (b)) (list a b)) nil))`,
		Expect: `"(lambda (&optional (a 1) b)
  (list a b))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 15)) (pretty-print (lambda (a b) "black sheep" (* (+ a b) (- a b))) nil))`,
		Expect: `"(lambda (a b)
  "black sheep"
  (* (+ a b)
     (- a b)))
"`,
	}).Test(t)
	// Not a valid lambda but still want to try and print.
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 15)) (pretty-print (lambda nil (princ 'hi)) nil))`,
		Expect: `"(lambda nil
  (princ 'hi))
"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30)) (pretty-print (lambda (a b) (format t "--- ~A ---~A~%" a b)) nil))`,
		Expect: `"(lambda (a b)
  (format t
          "--- ~A ---~A~%"
          a
          b))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30)) (pretty-print (lambda (a b) "black sheep" (* (+ a b) (- a b))) nil))`,
		Expect: `"(lambda (a b)
  "black sheep"
  (* (+ a b) (- a b)))
"`,
	}).Test(t)
}

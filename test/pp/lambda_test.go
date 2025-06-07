// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestLambdaNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 15)) (pretty-print (lambda () (princ 'hi)) nil))`,
		Expect: `"(lambda ()
  (princ 'hi))
"`,
	}).Test(t)
	// Not a valid lambda but still want to try and print.
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 15)) (pretty-print (lambda nil (princ 'hi)) nil))`,
		Expect: `"(lambda nil
  (princ 'hi))
"`,
	}).Test(t)
}

func TestLambdaOptionalArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30)) (pretty-print (lambda (&optional (a 1) (b)) (list a b)) nil))`,
		Expect: `"(lambda (&optional (a 1) b)
  (list a b))
"`,
	}).Test(t)
}

func TestLambdaDocs(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 15)) (pretty-print (lambda (a b) "black sheep" (* (+ a b) (- a b))) nil))`,
		Expect: `"(lambda (a b)
  "black sheep"
  (* (+ a b)
     (- a b)))
"`,
	}).Test(t)
}

func TestLambdaLongerLine(t *testing.T) {
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

func TestLambdaVar(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30)
                       (lam (lambda (a &optional (b 2)) (+ a b))))
                   (pretty-print lam nil))`,
		Expect: `"(lambda (a &optional (b 2))
  (+ a b))
"`,
	}).Test(t)
}

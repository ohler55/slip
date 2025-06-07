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

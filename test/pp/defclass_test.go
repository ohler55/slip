// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDefclassBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60))
                   (pretty-print
                    (defclass strawberry (berry)
                      ((size :initform "medium" :initarg :size :reader get-size :writer set-size)
                       color)
                      (:documentation "Strawberry icecream")
                      (:default-initargs :size "large" :flavor "sweet"))
                    nil))`,
		Expect: `"(defclass strawberry (berry)
  ((size
    :initform "medium"
    :initarg :size
    :reader get-size
    :writer set-size)
   color)
  (:documentation "Strawberry icecream")
  (:default-initargs :size "large" :flavor "sweet"))
"`,
	}).Test(t)
}

func TestDefclassTight(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20))
                   (pretty-print
                    (defclass strawberry (berry)
                      ((size :initform "medium" :initarg :size :reader get-size :writer set-size)
                       color)
                      (:documentation "Strawberry icecream")
                      (:default-initargs :size "large" :flavor "a fresh sweet taste"))
                    nil))`,
		Expect: `"(defclass strawberry
  (berry)
  ((size
    :initform
    "medium"
    :initarg :size
    :reader get-size
    :writer set-size)
   color)
  (:documentation
   "Strawberry
    icecream")
  (:default-initargs
   :size "large"
   :flavor
   "a fresh sweet taste"))
"`,
	}).Test(t)
}

func TestDefclassOddOption(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60))
                   (pretty-print
                    (defclass strawberry nil nil
                      (:default-initargs :size "large" :flavor))
                    nil))`,
		Expect: `"(defclass strawberry ()
  ()
  (:default-initargs :size "large" :flavor))
"`,
	}).Test(t)
}

func TestDefclassBadParts(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60))
                   (pretty-print
                    (defclass strawberry super slots bad)
                    nil))`,
		Expect: `"(defclass strawberry super
  slots
  bad)
"`,
	}).Test(t)
}

func TestDefclassBadSlot(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60))
                   (pretty-print
                    (defclass strawberry () (7))
                    nil))`,
		Expect: `"(defclass strawberry ()
  (7))
"`,
	}).Test(t)
}

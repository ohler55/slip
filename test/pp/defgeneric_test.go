// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDefgeneric80(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 80))
                   (pretty-print
                    (defgeneric quux (x y)
                      (:documentation "quack quack")
                      (:method ((x real) (y real)) (+ x y))
                      (:method :before ((x real) (y real)) (print "before"))
                      (:method :after ((x real) (y real)) (print "after"))
                      (:method :around ((x real) (y real)) (print "around") (call-next-method x y))) nil))`,
		Expect: `"(defgeneric quux (x y)
  (:documentation "quack quack")
  (:method ((x real) (y real))
    (+ x y))
  (:method :before ((x real) (y real))
    (print "before"))
  (:method :after ((x real) (y real))
    (print "after"))
  (:method :around ((x real) (y real))
    (print "around")
    (call-next-method x y)))
"`,
	}).Test(t)
}

func TestDefgeneric30(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30))
                   (pretty-print
                    (defgeneric quux (x y)
                      (:documentation "quack quack")
                      (:method ((x real) (y real)) "duck" (+ x y))
                      (:method :before ((x real) (y real)) (print "before"))
                      (:method :after ((x real) (y real)) (print "after a long time or something"))
                      (:method :around ((x real) (y real)) (print "around") (call-next-method x y))) nil))`,
		Expect: `"(defgeneric quux (x y)
  (:documentation
   "quack quack")
  (:method ((x real) (y real))
    "duck"
    (+ x y))
  (:method :before
      ((x real) (y real))
    (print "before"))
  (:method :after
      ((x real) (y real))
    (print
     "after a long time or something"))
  (:method :around
      ((x real) (y real))
    (print "around")
    (call-next-method x y)))
"`,
	}).Test(t)
}

func TestDefgeneric20(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20))
                   (pretty-print
                    (defgeneric quux (x y)
                      (:documentation "quack quack")
                      (:method ((x real) (y real)) (+ x y))
                      (:method :before ((x real) (y real)) (print "before"))
                      (:method :after ((x real) (y real)) (print "after"))
                      (:method :around ((x real) (y real)) "duux" (print "around") (call-next-method x y))) nil))`,
		Expect: `"(defgeneric quux
    (x y)
  (:documentation
   "quack quack")
  (:method
      ((x real)
       (y real))
    (+ x y))
  (:method :before
      ((x real)
       (y real))
    (print "before"))
  (:method :after
      ((x real)
       (y real))
    (print "after"))
  (:method :around
      ((x real)
       (y real))
    "duux"
    (print "around")
    (call-next-method
     x
     y)))
"`,
	}).Test(t)
}

func TestDefgeneric15(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 15))
                   (pretty-print
                    (defgeneric quux (x y)
                      (:documentation "quack quack")
                      (:method ((x real) (y real)) (+ x y))
                      (:method :before ((x real) (y real)) (print "before"))
                      (:method :after ((x real) (y real)) (print "after"))
                      (:method :around ((x real) (y real)) (print "around") (call-next-method x y))) nil))`,
		Expect: `"(defgeneric
    quux
    (x y)
  (:documentation
   "quack quack")
  (:method
      ((x real)
       (y real))
    (+ x y))
  (:method
      :before
      ((x real)
       (y real))
    (print
     "before"))
  (:method :after
      ((x real)
       (y real))
    (print
     "after"))
  (:method
      :around
      ((x real)
       (y real))
    (print
     "around")
    (call-next-method
     x
     y)))
"`,
	}).Test(t)
}

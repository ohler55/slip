// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefmethodBasic(t *testing.T) {
	defer undefFlavor("blueberry")
	defer undefFlavor("berry")

	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor berry (color) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables)
(defmethod (berry :rot) () "When berries rot they turn brown." (setq color 'brown))
(defmethod (berry :before :color) () (princ "berry before color") (terpri))
(defmethod (berry :after :rot) () (princ "berry after rot") (terpri))

(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () "Berries that are blue." (princ "blueberry before rot") (terpri))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot") (terpri))
`, scope).Eval(scope, nil)

	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60)) (pretty-print berry:primary:rot nil))`,
		Expect: `"(defmethod (berry :primary :rot) ()
  "When berries rot they turn brown."
  (setq color 'brown))
"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30)) (pretty-print berry:primary:rot nil))`,
		Expect: `"(defmethod (berry :primary
            :rot)
    ()
  "When berries rot they turn
   brown."
  (setq color 'brown))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30)) (pretty-print berry:whopper:rot nil))`,
		Expect: `"nil
"`,
	}).Test(t)
}

func TestDefmethodLongLine(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30))
                   (pretty-print (defmethod (quux :before :qq) (a)
                                   (quack (quack (quack (quack a))))) nil))`,
		Expect: `"(defmethod (quux :before :qq)
    (a)
  (quack
   (quack (quack (quack a)))))
"`,
	}).Test(t)
}

func TestDefmethodGeneric(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 80))
                   (pretty-print (defmethod quux :before ((x fixnum) (y real))
                                   "quack quack" (+ x y)) nil))`,
		Expect: `"(defmethod quux :before ((x fixnum) (y real))
  "quack quack"
  (+ x y))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30))
                   (pretty-print (defmethod quux :before ((x fixnum) (y real))
                                   "quack quack" (+ x y)) nil))`,
		Expect: `"(defmethod quux :before
    ((x fixnum) (y real))
  "quack quack"
  (+ x y))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 20))
                   (pretty-print (defmethod quux :before ((x fixnum) (y real))
                                   "quack quack" (+ x y)) nil))`,
		Expect: `"(defmethod quux
    :before
    ((x fixnum)
     (y real))
  "quack quack"
  (+ x y))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 10))
                   (pretty-print (defmethod quux :before ((x fixnum) (y real))
                                   "quack quack" (+ x y)) nil))`,
		Expect: `"(defmethod
    quux
    :before
    ((x
      fixnum)
     (y
      real))
  "quack
   quack"
  (+ x y))
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 80))
                   (pretty-print (defmethod quux ((x t))
                                   (+ x *some-long-global-variable*)) nil))`,
		Expect: `"(defmethod quux ((x t))
  (+ x *some-long-global-variable*))
"`,
	}).Test(t)
}

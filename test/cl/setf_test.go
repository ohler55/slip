// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSetfSymbol(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Symbol("x"), slip.Fixnum(7)}),
		String: "(setf x 7)",
		Simple: []interface{}{"setf", "x", 7},
		Eval:   slip.Fixnum(7),
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
}

func TestSetfPlacer(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{slip.Fixnum(7), slip.Fixnum(8)})
	car := slip.NewFunc("car", slip.List{slip.Symbol("target")})
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{car, slip.Fixnum(9)}),
		String: "(setf (car target) 9)",
		Simple: []interface{}{"setf", []interface{}{"car", "target"}, 9},
		Eval:   slip.Fixnum(9),
	}).Test(t)
	tt.Equal(t, slip.List{slip.Fixnum(9), slip.Fixnum(8)}, scope.Get(slip.Symbol("target")))
}

func TestSetfPlacerList(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{slip.Fixnum(6), slip.Fixnum(7), slip.Fixnum(8)})
	car := slip.NewFunc("car", slip.List{slip.List{slip.Symbol("cdr"), slip.Symbol("target")}})
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{car, slip.Fixnum(9)}),
		String: "(setf (car (cdr target)) 9)",
		Simple: []interface{}{"setf", []interface{}{"car", []interface{}{"cdr", "target"}}, 9},
		Eval:   slip.Fixnum(9),
	}).Test(t)
	tt.Equal(t, slip.List{slip.Fixnum(6), slip.Fixnum(9), slip.Fixnum(8)}, scope.Get(slip.Symbol("target")))
}

func TestSetfNotPairs(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Symbol("x")}),
		String: "(setf x)",
		Simple: []interface{}{"setf", "x"},
		Panics: true,
	}).Test(t)
}

func TestSetfNotPlacer(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(7), slip.Fixnum(8)}),
		String: "(setf 7 8)",
		Simple: []interface{}{"setf", 7, 8},
		Panics: true,
	}).Test(t)
}

func TestSetfList(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{slip.Fixnum(7), slip.Fixnum(8)})
	car := slip.List{slip.Symbol("car"), slip.Symbol("target")}
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{car, slip.Fixnum(9)}),
		String: "(setf (car target) 9)",
		Simple: []interface{}{"setf", []interface{}{"car", "target"}, 9},
		Eval:   slip.Fixnum(9),
	}).Test(t)
	tt.Equal(t, slip.List{slip.Fixnum(9), slip.Fixnum(8)}, scope.Get(slip.Symbol("target")))
}

func TestSetfGenericSetf(t *testing.T) {
	slip.CurrentPackage.Undefine("(setf buux)")
	(&sliptest.Function{
		Source: `(defmethod (setf buux) ((v fixnum) (lst list)) (setf (nth 1 lst) v))`,
		Expect: `/#<method \(setf buux\) \(\(v fixnum\) \(lst list\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((x '(1 2 3)))
                   (setf (buux x) 5)
                   x)`,
		Expect: `(1 5 3)`,
	}).Test(t)
}

// Taken from test/defmethod_test so that coverage is also applied to this
// package.
func TestSetfDefmethodCaller(t *testing.T) {
	for _, name := range []string{"quux",
		"quux-x", "quux-y",
		"quux-set-x", "quux-set-y",
		"quux-acc-x", "quux-acc-y",
		"(setf-acc-x)", "(setf-acc-y)",
	} {
		slip.CurrentPackage.Remove(name)
	}
	(&sliptest.Function{
		Source: `(progn
                   (defclass quux ()
                     ((x :initform 7 :reader quux-x :writer quux-set-x :accessor quux-acc-x :gettable t :settable t)
                      (y :initform 2 :reader quux-y :writer quux-set-y :accessor quux-acc-y :allocation :class)))
                   (let* ((q (make-instance 'quux))
                          (result (list (quux-x q))))
                     (quux-set-x q 3)
                     (addf result (quux-x q))
                     (setf (quux-acc-x q) 4)
                     (addf result (quux-x q))
                     (quux-set-y q 4)
                     (addf result (quux-y q))
                     (setf (quux-acc-y q) 6)
                     (addf result (quux-y q))
                     (send q :set-x 1)
                     (addf result (send q :x))
                     result))`,
		Expect: "(7 3 4 4 6 1)",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (quux-acc-x) 4)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)

}

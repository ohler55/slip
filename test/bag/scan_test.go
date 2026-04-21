// Copyright (c) 2026, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBagScanEmptyMap(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{}"))
                       result)
                   (bag-scan bag '(lambda (p v) (setq result (add result (list p v)))))
                   result)`,
		Validate: func(t *testing.T, v slip.Object) {
			lst := v.(slip.List)
			tt.Equal(t, 1, len(lst))
			tt.Equal(t, `/\("\$" #<bag-flavor [0-9a-f]+>\)/`, lst[0].String())
		},
	}).Test(t)
}

func TestBagScanMap(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:7}"))
                       result)
                   (bag-scan bag (lambda (p v) (setq result (add result (list p v)))))
                   result)`,
		Validate: func(t *testing.T, v slip.Object) {
			lst := v.(slip.List)
			tt.Equal(t, 2, len(lst))
			tt.Equal(t, `/\("\$" #<bag-flavor [0-9a-f]+>\)/`, lst[0].String())
			tt.Equal(t, `("$.a" 7)`, lst[1].String())
		},
	}).Test(t)
}

func TestBagScanMapLeaves(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:7}"))
                       result)
                   (bag-scan bag (lambda (p v) (setq result (add result (list p v)))) :leaves-only t)
                   result)`,
		Expect: `(("$.a" 7))`,
	}).Test(t)
}

func TestBagScanSend(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:7}"))
                       result)
                   (send bag :scan (lambda (p v) (setq result (add result (list p v)))) :leaves-only t)
                   result)`,
		Expect: `(("$.a" 7))`,
	}).Test(t)
}

func TestBagScanFunc(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq result '())`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(defun scan-test (p v) (setq result (add result (list p v))))`, scope).Eval(scope, nil)
	(&sliptest.Function{
		Source: `(bag-scan (make-bag "{a:7}") 'scan-test :leaves-only t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(("$.a" 7))`, scope.Get(slip.Symbol("result")).String())
}

func TestBagScanNotBag(t *testing.T) {
	(&sliptest.Function{
		Source:    `(bag-scan t (lambda (p v) nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBagScanNotFunc(t *testing.T) {
	(&sliptest.Function{
		Source:    `(bag-scan (make-bag "{}") t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

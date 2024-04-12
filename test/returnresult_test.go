// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReturnResult(t *testing.T) {
	rr := slip.ReturnResult{Tag: slip.Symbol("tug"), Result: slip.Fixnum(3)}
	(&sliptest.Object{
		Target:    &rr,
		String:    "#<return-result tug>",
		Simple:    map[string]any{"tag": "tug", "result": 3},
		Hierarchy: "t",
		Equals: []*sliptest.EqTest{
			{Other: &rr, Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
		},
		Eval: &rr,
	}).Test(t)
}

func TestReturnResultLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(apply (lambda ()
                  (return 7)
                  'end) nil)`,
		Expect: "7",
	}).Test(t)
}

func TestReturnResultDefun(t *testing.T) {
	(&sliptest.Function{
		Source: `(defun rr-fun ()
                  (return-from rr-fun 7)
                  'end)
                 (rr-fun)`,
		Expect: "7",
	}).Test(t)
}

func TestReturnResultNotFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(apply (lambda ()
                  (return-from nada 7)
                  'end) nil)`,
		PanicType: slip.ControlErrorSymbol,
	}).Test(t)
}

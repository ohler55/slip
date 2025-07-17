// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSimpleConditionFormatControlExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(simple-condition-format-control
                  (make-condition 'simple-condition
                                  :format-control "~A test"
                                  :format-arguments '(simple)))`,
		Expect: `"~A test"`,
	}).Test(t)
}

func TestSimpleConditionFormatControlSub(t *testing.T) {
	(&sliptest.Function{
		Source: `(simple-condition-format-control
                  (make-condition 'simple-error
                                  :format-control "~A test"
                                  :format-arguments '(simple)))`,
		Expect: `"~A test"`,
	}).Test(t)
}

func TestSimpleConditionFormatControlNotSimpleCondition(t *testing.T) {
	(&sliptest.Function{
		Source:    `(simple-condition-format-control (make-condition 'condition :format-control "x"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

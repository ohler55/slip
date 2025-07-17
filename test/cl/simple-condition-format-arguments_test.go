// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSimpleConditionFormatArgumentsExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(simple-condition-format-arguments
                  (make-condition 'simple-condition
                                  :format-control "~A test"
                                  :format-arguments '(simple)))`,
		Expect: "(simple)",
	}).Test(t)
}

func TestSimpleConditionFormatArgumentsSub(t *testing.T) {
	(&sliptest.Function{
		Source: `(simple-condition-format-arguments
                  (make-condition 'simple-error
                                  :format-control "~A test"
                                  :format-arguments '(simple)))`,
		Expect: "(simple)",
	}).Test(t)
}

func TestSimpleConditionFormatArgumentsNotSimpleCondition(t *testing.T) {
	(&sliptest.Function{
		Source:    `(simple-condition-format-arguments (make-condition 'condition :format-arguments '(2)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

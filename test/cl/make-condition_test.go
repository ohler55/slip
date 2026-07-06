// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeConditionBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(make-condition 'unbound-slot :name 'slop :instance 'cymbol)",
		Expect: "/^#<unbound-slot [0-9a-f]+>$/",
	}).Test(t)
	(&sliptest.Function{
		Source: "(make-condition :unbound-slot :name 'slop :instance 'cymbol)",
		Expect: "/^#<unbound-slot [0-9a-f]+>$/",
	}).Test(t)
}

func TestMakeConditionNotCondition(t *testing.T) {
	(&sliptest.Function{
		Source:    "(make-condition 'foo)",
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    "(make-condition 7)",
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

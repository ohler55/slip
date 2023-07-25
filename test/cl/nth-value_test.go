// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNthValueNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(nth-value 0 nil)",
		Expect: "nil",
	}).Test(t)
}

func TestNthValueOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(nth-value 1 (values 'a 'b 'c))",
		Expect: "b",
	}).Test(t)
}

func TestNthValueBounds(t *testing.T) {
	(&sliptest.Function{
		Source: "(nth-value 3 (values 'a 'b 'c))",
		Expect: "nil",
	}).Test(t)
}

func TestNthValueBadIndex(t *testing.T) {
	(&sliptest.Function{
		Source: "(nth-value t (values 'a 'b 'c))",
		Panics: true,
	}).Test(t)
}

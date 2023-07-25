// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestValuesListNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(values-list nil)",
		Expect: "",
	}).Test(t)
}

func TestValuesListBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(values-list '(a b c))",
		Expect: "a, b, c",
	}).Test(t)
}

func TestValuesListNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(values-list t)",
		Panics: true,
	}).Test(t)
}

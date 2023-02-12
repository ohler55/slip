// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCddddrEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddddr nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCddddrFoundCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddddr '(a . (b . (c . (d . e)))))",
		Expect: "e",
	}).Test(t)
}

func TestCddddrFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddddr '(a b c d e))",
		Expect: "(e)",
	}).Test(t)
}

func TestCddddrWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddddr)",
		Panics: true,
	}).Test(t)
}

func TestCddddrWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddddr t)",
		Panics: true,
	}).Test(t)
}

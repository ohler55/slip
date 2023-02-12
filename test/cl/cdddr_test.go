// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCdddrEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCdddrFoundCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr '(a . (b . (c . d))))",
		Expect: "d",
	}).Test(t)
}

func TestCdddrFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr '(a b c d))",
		Expect: "(d)",
	}).Test(t)
}

func TestCdddrWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr)",
		Panics: true,
	}).Test(t)
}

func TestCdddrWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr t)",
		Panics: true,
	}).Test(t)
}

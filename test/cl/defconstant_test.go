// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefconstantBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(defconstant *defconstant-test-basic* (+ 3 4) "A test doc.")`,
		Expect: `*defconstant-test-basic*`,
	}).Test(t)
	tt.Equal(t, "A test doc.", slip.DescribeVar(slip.Symbol("*defconstant-test-basic*")))
	(&sliptest.Function{
		Source: `(defconstant *defconstant-test-basic* 7)`,
		Expect: `*defconstant-test-basic*`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(defconstant *defconstant-test-basic* 6)`,
		Panics: true,
	}).Test(t)
}

func TestDefconstantNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(defconstant t 3)`,
		Panics: true,
	}).Test(t)
}

func TestDefconstantDocNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(defconstant *nothing* 3 t)`,
		Panics: true,
	}).Test(t)
}

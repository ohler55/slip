// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSliptestFunctionValidate(t *testing.T) {
	(&sliptest.Function{
		Source: "(setq x 3)",
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, slip.Fixnum(3), v)
		},
	}).Test(t)
}

func TestSliptestFunctionReadably(t *testing.T) {
	(&sliptest.Function{
		Source:   "12.345e10",
		Expect:   "1.2345d+11",
		Readably: true,
	}).Test(t)
}

func TestSliptestFunctionPanics(t *testing.T) {
	(&sliptest.Function{
		Source: "(/ 1 0) ",
		Panics: true,
	}).Test(t)
}

func TestSliptestFunctionPanicType(t *testing.T) {
	(&sliptest.Function{
		Source:    "(/ 1 0)",
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
}

func TestSliptestObjectPanics(t *testing.T) {
	(&sliptest.Object{
		Target: slip.Symbol("not-bound"),
		String: "not-bound",
		Simple: "not-bound",
		Panics: true,
	}).Test(t)
}

func TestSliptestObjectPanicType(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Symbol("not-bound"),
		String:    "not-bound",
		Simple:    "not-bound",
		PanicType: slip.Symbol("unbound-variable"),
	}).Test(t)
}

func TestSliptestMacroBasic(t *testing.T) {
	// Define a macro and call it in one source string
	(&sliptest.Macro{
		Source: "(defmacro inc1 (x) `(1+ ,x))",
		Expect: "inc1",
	}).Test(t)
}

func TestSliptestMacroMultipleForms(t *testing.T) {
	// The key difference from Function: Macro can evaluate multiple forms
	// where earlier forms define macros used by later forms
	(&sliptest.Macro{
		Source: `
(defmacro double (x) ` + "`" + `(* 2 ,x))
(double 5)`,
		Expect: "10",
	}).Test(t)
}

func TestSliptestMacroValidate(t *testing.T) {
	(&sliptest.Macro{
		Source: "(defmacro triple (x) `(* 3 ,x)) (triple 7)",
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, slip.Fixnum(21), v)
		},
	}).Test(t)
}

func TestSliptestMacroReadably(t *testing.T) {
	(&sliptest.Macro{
		Source:   "(+ 1.5d0 2.5d0)",
		Expect:   "4d+00",
		Readably: true,
	}).Test(t)
}

func TestSliptestMacroArray(t *testing.T) {
	(&sliptest.Macro{
		Source: "#(1 2 3)",
		Expect: "#(1 2 3)",
		Array:  true,
	}).Test(t)
}

func TestSliptestMacroPanics(t *testing.T) {
	(&sliptest.Macro{
		Source: "(/ 1 0)",
		Panics: true,
	}).Test(t)
}

func TestSliptestMacroPanicType(t *testing.T) {
	(&sliptest.Macro{
		Source:    "(/ 1 0)",
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
}

func TestSliptestMacroPanicTypeValidate(t *testing.T) {
	(&sliptest.Macro{
		Source:    "(error \"test error\")",
		PanicType: slip.Symbol("error"),
		Validate: func(t *testing.T, v slip.Object) {
			tt.NotNil(t, v)
		},
	}).Test(t)
}

func TestSliptestMacroWithScope(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString("(defmacro add10 (x) `(+ 10 ,x))", scope).Eval(scope, nil)
	(&sliptest.Macro{
		Scope:  scope,
		Source: "(add10 32)",
		Expect: "42",
	}).Test(t)
}

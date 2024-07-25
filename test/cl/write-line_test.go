// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWriteLineOkay(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(write-line "abcdef" out :start 1 :end 3)`).Eval(scope, nil)

	tt.Equal(t, slip.String("abcdef"), result)
	tt.Equal(t, "bc\n", out.String())
}

func TestWriteLineNilEnd(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(write-line "abcdef" out :end nil)`).Eval(scope, nil)

	tt.Equal(t, slip.String("abcdef"), result)
	tt.Equal(t, "abcdef\n", out.String())
}

func TestWriteLineNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-line "abc" t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteLineNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-line t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteLineBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-line "abc" :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(write-line "abc" :start 4)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestWriteLineBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-line "abc" :end t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(write-line "abc" :start 2 :end 1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestWriteLineWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(write-line "abc" out)`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

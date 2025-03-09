// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWriteStringOkay(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(write-string "abcdef" out :start 1 :end 3)`, scope).Eval(scope, nil)

	tt.Equal(t, slip.String("abcdef"), result)
	tt.Equal(t, "bc", out.String())
}

func TestWriteStringNilEnd(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(write-string "abcdef" out :end nil)`, scope).Eval(scope, nil)

	tt.Equal(t, slip.String("abcdef"), result)
	tt.Equal(t, "abcdef", out.String())
}

func TestWriteStringNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-string "abc" t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteStringNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-string t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteStringBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-string "abc" :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(write-string "abc" :start 4)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestWriteStringBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-string "abc" :end t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(write-string "abc" :start 2 :end 1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestWriteStringWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(write-string "abc" out)`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

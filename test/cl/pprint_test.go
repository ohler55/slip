// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestPprintStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString("(pprint 123 out)").Eval(scope)

	tt.Equal(t, slip.Novalue, result)
	tt.Equal(t, "\n123", out.String())
}

func TestPprintStreamString(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(pprint "abc" out)`).Eval(scope)

	tt.Equal(t, slip.Novalue, result)
	tt.Equal(t, "\n\"abc\"", out.String())
}

func TestPprintStdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadString("(pprint 123)").Eval(scope)

	tt.Equal(t, slip.Novalue, result)
	tt.Equal(t, "\n123", out.String())
}

func TestPprintArgCount(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString("(pprint)").Eval(slip.NewScope()) })
}

func TestPprintBadStream(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString("(pprint 123 t)").Eval(slip.NewScope()) })
}

func TestPprintWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(pprint 123 out)").Eval(scope) })
}

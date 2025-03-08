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
	result := slip.ReadString("(pprint 123 out)", scope).Eval(scope, nil)

	tt.Equal(t, slip.Novalue, result)
	tt.Equal(t, "\n123", out.String())
}

func TestPprintStreamString(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(pprint "abc" out)`, scope).Eval(scope, nil)

	tt.Equal(t, slip.Novalue, result)
	tt.Equal(t, "\n\"abc\"", out.String())
}

func TestPprintStdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadString("(pprint 123)", scope).Eval(scope, nil)

	tt.Equal(t, slip.Novalue, result)
	tt.Equal(t, "\n123", out.String())
}

func TestPprintArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(pprint)", scope).Eval(scope, nil) })
}

func TestPprintBadStream(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(pprint 123 t)", scope).Eval(scope, nil) })
}

func TestPprintWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(pprint 123 out)", scope).Eval(scope, nil) })
}

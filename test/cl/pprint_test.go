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
	result := slip.ReadEvalString("(pprint 123 out)", scope)

	tt.Equal(t, slip.Novalue, result)
	tt.Equal(t, "\n123", out.String())
}

func TestPprintStreamString(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadEvalString(`(pprint "abc" out)`, scope)

	tt.Equal(t, slip.Novalue, result)
	tt.Equal(t, "\n\"abc\"", out.String())
}

func TestPprintStdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadEvalString("(pprint 123)", scope)

	tt.Equal(t, slip.Novalue, result)
	tt.Equal(t, "\n123", out.String())
}

func TestPprintArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(pprint)", scope) })
}

func TestPprintBadStream(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(pprint 123 t)", scope) })
}

func TestPprintWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadEvalString("(pprint 123 out)", scope) })
}

// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestPrin1Stream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadEvalString("(prin1 123 out)", scope)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "123", out.String())
}

func TestPrin1StreamString(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadEvalString(`(prin1 "abc" out)`, scope)

	tt.Equal(t, slip.String("abc"), result)
	tt.Equal(t, `"abc"`, out.String())
}

func TestPrin1Stdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadEvalString("(prin1 123)", scope)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "123", out.String())
}

func TestPrin1ArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(prin1)", scope) })
}

func TestPrin1BadStream(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(prin1 123 t)", scope) })
}

func TestPrin1WriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadEvalString("(prin1 123 out)", scope) })
}

func TestPrin1WriteStructWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadEvalString(`(defstruct (p1wspf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope)
	var b bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadEvalString("(prin1 (make-p1wspf :x 1) out)", scope)
	tt.Equal(t, "x", b.String())
}

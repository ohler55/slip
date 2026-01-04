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
	result := slip.ReadString("(prin1 123 out)", scope).Eval(scope, nil)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "123", out.String())
}

func TestPrin1StreamString(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(prin1 "abc" out)`, scope).Eval(scope, nil)

	tt.Equal(t, slip.String("abc"), result)
	tt.Equal(t, `"abc"`, out.String())
}

func TestPrin1Stdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadString("(prin1 123)", scope).Eval(scope, nil)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "123", out.String())
}

func TestPrin1ArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(prin1)", scope).Eval(scope, nil) })
}

func TestPrin1BadStream(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(prin1 123 t)", scope).Eval(scope, nil) })
}

func TestPrin1WriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(prin1 123 out)", scope).Eval(scope, nil) })
}

func TestPrin1WriteStructWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (p1wspf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope).Eval(scope, nil)
	var b bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadString("(prin1 (make-p1wspf :x 1) out)", scope).Eval(scope, nil)
	tt.Equal(t, "x", b.String())
}

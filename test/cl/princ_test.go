// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"bytes"
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestPrincStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString("(princ 123 out)", scope).Eval(scope, nil)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "123", out.String())
}

func TestPrincStreamString(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(princ "abc" out)`, scope).Eval(scope, nil)

	tt.Equal(t, slip.String("abc"), result)
	tt.Equal(t, "abc", out.String())
}

func TestPrincStdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadString("(princ 123)", scope).Eval(scope, nil)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "123", out.String())
}

func TestPrincArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(princ)", scope).Eval(scope, nil) })
}

func TestPrincBadStream(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(princ 123 t)", scope).Eval(scope, nil) })
}

type badWriter int

func (w badWriter) Write([]byte) (int, error) {
	return 0, fmt.Errorf("oops")
}

func TestPrincWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(princ 123 out)", scope).Eval(scope, nil) })
}

func TestPrincWriteStructWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (pcwspf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope).Eval(scope, nil)
	var b bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadString("(princ (make-pcwspf :x 1) out)", scope).Eval(scope, nil)
	tt.Equal(t, "x", b.String())
}

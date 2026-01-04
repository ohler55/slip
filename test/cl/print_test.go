// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestPrintStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString("(print 123 out)", scope).Eval(scope, nil)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "\n123 ", out.String())
}

func TestPrintStreamString(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(print "abc" out)`, scope).Eval(scope, nil)

	tt.Equal(t, slip.String("abc"), result)
	tt.Equal(t, "\n\"abc\" ", out.String())
}

func TestPrintStdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadString("(print 123)", scope).Eval(scope, nil)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "\n123 ", out.String())
}

func TestPrintArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(print)", scope).Eval(scope, nil) })
}

func TestPrintBadStream(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(print 123 t)", scope).Eval(scope, nil) })
}

func TestPrintWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(print 123 out)", scope).Eval(scope, nil) })
}

func TestPrintWriteStructWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (pwspf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope).Eval(scope, nil)
	var b bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadString("(print (make-pwspf :x 1) out)", scope).Eval(scope, nil)
	tt.Equal(t, "\nx ", b.String())
}

func TestPrintWriteFailWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (pwfpf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope).Eval(scope, nil)
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(print (make-pwfpf :x 1) out)", scope).Eval(scope, nil) })
}

func TestPrintWriteFailWithPrintObject(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (pwfpo (:print-object (lambda (obj str) (princ "x" str)))) x)`, scope).Eval(scope, nil)
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(print (make-pwfpo :x 1) out)", scope).Eval(scope, nil) })
}

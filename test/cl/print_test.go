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
	result := slip.ReadEvalString("(print 123 out)", scope)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "\n123 ", out.String())
}

func TestPrintStreamString(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadEvalString(`(print "abc" out)`, scope)

	tt.Equal(t, slip.String("abc"), result)
	tt.Equal(t, "\n\"abc\" ", out.String())
}

func TestPrintStdout(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: &out}
	result := slip.ReadEvalString("(print 123)", scope)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "\n123 ", out.String())
}

func TestPrintArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(print)", scope) })
}

func TestPrintBadStream(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(print 123 t)", scope) })
}

func TestPrintWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadEvalString("(print 123 out)", scope) })
}

func TestPrintWriteStructWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadEvalString(`(defstruct (pwspf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope)
	var b bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadEvalString("(print (make-pwspf :x 1) out)", scope)
	tt.Equal(t, "\nx ", b.String())
}

func TestPrintWriteFailWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadEvalString(`(defstruct (pwfpf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope)
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadEvalString("(print (make-pwfpf :x 1) out)", scope) })
}

func TestPrintWriteFailWithPrintObject(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadEvalString(`(defstruct (pwfpo (:print-object (lambda (obj str) (princ "x" str)))) x)`, scope)
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadEvalString("(print (make-pwfpo :x 1) out)", scope) })
}

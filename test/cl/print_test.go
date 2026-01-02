// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

// failAfterNWriter fails after n successful writes
type failAfterNWriter struct {
	n int
}

func (w *failAfterNWriter) Write(p []byte) (int, error) {
	if w.n <= 0 {
		return 0, fmt.Errorf("write failed")
	}
	w.n--
	return len(p), nil
}

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

func TestPrintWriteFailWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (pwfpf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`, scope).Eval(scope, nil)
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(print (make-pwfpf :x 1) out)", scope).Eval(scope, nil) })
}

func TestPrintWriteFailWithPrintObject(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (pwfpo (:print-object (lambda (obj str) (princ "x" str)))) x)`, scope).Eval(scope, nil)
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(print (make-pwfpo :x 1) out)", scope).Eval(scope, nil) })
}

// Test trailing space write failure after print-function runs
func TestPrintWriteFailSpaceWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (pwfspf (:print-function (lambda (obj str depth) nil))) x)`, scope).Eval(scope, nil)
	// Allow newline write (1), fail on space write
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &failAfterNWriter{n: 1}})
	tt.Panic(t, func() { _ = slip.ReadString("(print (make-pwfspf :x 1) out)", scope).Eval(scope, nil) })
}

// Test trailing space write failure after print-object runs
func TestPrintWriteFailSpaceWithPrintObject(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (pwfspo (:print-object (lambda (obj str) nil))) x)`, scope).Eval(scope, nil)
	// Allow newline write (1), fail on space write
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &failAfterNWriter{n: 1}})
	tt.Panic(t, func() { _ = slip.ReadString("(print (make-pwfspo :x 1) out)", scope).Eval(scope, nil) })
}

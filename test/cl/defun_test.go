// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestDefunBasic(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadEvalString(`
(defun funny (x)
  "Documentation here"
  x)
(funny 7)`, scope)
	tt.Equal(t, slip.Fixnum(7), result)
}

func TestDefunJustString(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadEvalString(`
(defun funny-hello () "hello")
(funny-hello)`, scope)
	tt.Equal(t, slip.String("hello"), result)
}

func TestDefunDefaultArgValue(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadEvalString(`
(defun funny2 (&optional (x 3)) x)
(funny2 7)`, scope)
	tt.Equal(t, slip.Fixnum(7), result)
	tt.Equal(t, slip.Fixnum(3), slip.ReadEvalString("(funny2)", scope))
}

func TestDefunDefaultArgNilValue(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadEvalString(`
(defun funny3 (&optional (x nil)) x)
(funny3 7)`, scope)
	tt.Equal(t, slip.Fixnum(7), result)
	tt.Equal(t, nil, slip.ReadEvalString("(funny3)", scope))
}

func TestDefunRest(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadEvalString(`
(defun the-rest (&rest args) args)
(the-rest 1 2 3)`, scope)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, result)
}

func TestDefunKey(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadEvalString(`
(defun key-rest (&key kee) kee)
(key-rest :kee 1)`, scope)
	tt.Equal(t, slip.Fixnum(1), result)
}

func TestDefunPre(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadEvalString(`
(defun aaa () (bbb))
(defun bbb () 7)
(aaa)`, scope)
	tt.Equal(t, slip.Fixnum(7), result)
}

func TestDefunClosure(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadEvalString(`
(let ((x 7))
 (defun clo () x))
(clo)
`, scope)
	tt.Equal(t, slip.Fixnum(7), result)
}

func TestDefunBadName(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defun t () nil)", scope) })
}

func TestDefunBadLambdaList(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defun bad t nil)", scope) })
}

func TestDefunBadLambdaListArg(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defun bad ((x)) nil)", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defun bad ((t 0)) nil)", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defun bad (t) nil)", scope) })
}

func TestDefunAgain(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let("*error-output*", &slip.OutputStream{Writer: &out})
	result := slip.ReadEvalString(`
(defun again () 3)
(defun again () 7)
(again)`, scope)
	tt.Equal(t, slip.Fixnum(7), result)
	tt.Equal(t, "WARNING: redefining common-lisp-user:again in defun\n", out.String())
}

func TestDefunTooManyArgs(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defun too-many () 3)
(too-many 7)`, scope)
	})
}

func TestDefunTooFewArgs(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defun too-few (x) x)
(too-few)`, scope)
	})
}

func TestDefunLocked(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defun setq () nil)", scope) })
}

func TestDefunInternal(t *testing.T) {
	scope := slip.NewScope()
	orig := slip.CurrentPackage
	defer func() {
		slip.RemovePackage(slip.FindPackage("internal-test"))
		scope.Set("*package*", orig)
	}()
	_ = slip.ReadEvalString(`(defpackage 'internal-test (:use "cl"))`, scope)
	_ = slip.ReadEvalString(`(in-package 'internal-test)`, scope)
	_ = slip.ReadEvalString(`(defun child-fun () 7)`, scope)
	_ = slip.ReadEvalString(`(defun parent-fun () (1+ (child-fun)))`, scope)
	_ = slip.ReadEvalString(`(export 'parent-fun)`, scope)

	result := slip.ReadEvalString(`(parent-fun)`, scope)
	tt.Equal(t, slip.Fixnum(8), result)

	_ = slip.ReadEvalString(`(in-package 'cl)`, scope)
	result = slip.ReadEvalString(`(internal-test:parent-fun)`, scope)
	tt.Equal(t, slip.Fixnum(8), result)
}

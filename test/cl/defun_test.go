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
	code := slip.ReadString(`
(defun funny (x)
  "Documentation here"
  x)
(funny 7)`, scope)
	tt.Equal(t, slip.Fixnum(7), code.Eval(scope, nil))
}

func TestDefunJustString(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defun funny-hello () "hello")
(funny-hello)`, scope)
	tt.Equal(t, slip.String("hello"), code.Eval(scope, nil))
}

func TestDefunDefaultArgValue(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defun funny2 (&optional (x 3)) x)
(funny2 7)`, scope)
	tt.Equal(t, slip.Fixnum(7), code.Eval(scope, nil))
	tt.Equal(t, slip.Fixnum(3), slip.ReadString("(funny2)", scope).Eval(scope, nil))
}

func TestDefunDefaultArgNilValue(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defun funny3 (&optional (x nil)) x)
(funny3 7)`, scope)
	tt.Equal(t, slip.Fixnum(7), code.Eval(scope, nil))
	tt.Equal(t, nil, slip.ReadString("(funny3)", scope).Eval(scope, nil))
}

func TestDefunRest(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defun the-rest (&rest args) args)
(the-rest 1 2 3)`, scope)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, code.Eval(scope, nil))
}

func TestDefunKey(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defun key-rest (&key kee) kee)
(key-rest :kee 1)`, scope)
	tt.Equal(t, slip.Fixnum(1), code.Eval(scope, nil))
}

func TestDefunPre(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defun aaa () (bbb))
(defun bbb () 7)
(aaa)`, scope)
	code.Compile()
	tt.Equal(t, slip.Fixnum(7), code.Eval(scope, nil))
}

func TestDefunClosure(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(let ((x 7))
 (defun clo () x))
(clo)
`, scope)
	tt.Equal(t, slip.Fixnum(7), code.Eval(scope, nil))
}

func TestDefunBadName(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(defun t () nil)", scope).Eval(scope, nil) })
}

func TestDefunBadLambdaList(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(defun bad t nil)", scope).Eval(scope, nil) })
}

func TestDefunBadLambdaListArg(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(defun bad ((x)) nil)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(defun bad ((t 0)) nil)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(defun bad (t) nil)", scope).Eval(scope, nil) })
}

func TestDefunAgain(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let("*error-output*", &slip.OutputStream{Writer: &out})
	code := slip.ReadString(`
(defun again () 3)
(defun again () 7)
(again)`, scope)
	tt.Equal(t, slip.Fixnum(7), code.Eval(scope, nil))
	tt.Equal(t, "WARNING: redefining common-lisp-user:again in defun\n", out.String())
}

func TestDefunTooManyArgs(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defun too-many () 3)
(too-many 7)`, scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestDefunTooFewArgs(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defun too-few (x) x)
(too-few)`, scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestDefunLocked(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(defun setq () nil)", scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestDefunInternal(t *testing.T) {
	scope := slip.NewScope()
	orig := slip.CurrentPackage
	defer func() {
		slip.RemovePackage(slip.FindPackage("internal-test"))
		scope.Set("*package*", orig)
	}()
	_ = slip.ReadString(`(defpackage 'internal-test (:use "cl"))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(in-package 'internal-test)`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(defun child-fun () 7)`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(defun parent-fun () (1+ (child-fun)))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(export 'parent-fun)`, scope).Eval(scope, nil)

	result := slip.ReadString(`(parent-fun)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(8), result)

	_ = slip.ReadString(`(in-package 'cl)`, scope).Eval(scope, nil)
	result = slip.ReadString(`(internal-test:parent-fun)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(8), result)
}

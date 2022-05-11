// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"bytes"
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestDefunBasic(t *testing.T) {
	code := slip.ReadString(`
(defun funny (x)
  "Documentation here"
  x)
(funny 7)`)
	scope := slip.NewScope()
	require.Equal(t, slip.Fixnum(7), code.Eval(scope))
}

func TestDefunDefaultArgValue(t *testing.T) {
	code := slip.ReadString(`
(defun funny2 (&optional (x 3)) x)
(funny2 7)`)
	scope := slip.NewScope()
	require.Equal(t, slip.Fixnum(7), code.Eval(scope))

	require.Equal(t, slip.Fixnum(3), slip.ReadString("(funny2)").Eval(scope))
}

func TestDefunDefaultArgNilValue(t *testing.T) {
	code := slip.ReadString(`
(defun funny3 (&optional (x nil)) x)
(funny3 7)`)
	scope := slip.NewScope()
	require.Equal(t, slip.Fixnum(7), code.Eval(scope))

	require.Equal(t, nil, slip.ReadString("(funny3)").Eval(scope))
}

func TestDefunRest(t *testing.T) {
	code := slip.ReadString(`
(defun the-rest (&rest args) args)
(the-rest 1 2 3)`)
	scope := slip.NewScope()
	require.Equal(t, slip.List{slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}, code.Eval(scope))
}

func TestDefunKey(t *testing.T) {
	code := slip.ReadString(`
(defun key-rest (&key kee) kee)
(key-rest :kee 1)`)
	scope := slip.NewScope()
	require.Equal(t, slip.Fixnum(1), code.Eval(scope))
}

func TestDefunPre(t *testing.T) {
	code := slip.ReadString(`
(defun aaa () (bbb))
(defun bbb () 7)
(aaa)`)
	scope := slip.NewScope()
	code.Compile()
	require.Equal(t, slip.Fixnum(7), code.Eval(scope))
}

func TestDefunClosure(t *testing.T) {
	code := slip.ReadString(`
(let ((x 7))
 (defun clo () x))
(clo)
`)
	scope := slip.NewScope()
	require.Equal(t, slip.Fixnum(7), code.Eval(scope))
}

func TestDefunBadName(t *testing.T) {
	require.Panics(t, func() { _ = slip.ReadString("(defun t () nil)").Eval(slip.NewScope()) })
}

func TestDefunBadLambdaList(t *testing.T) {
	require.Panics(t, func() { _ = slip.ReadString("(defun bad t nil)").Eval(slip.NewScope()) })
}

func TestDefunBadLambdaListArg(t *testing.T) {
	require.Panics(t, func() { _ = slip.ReadString("(defun bad ((x)) nil)").Eval(slip.NewScope()) })
	require.Panics(t, func() { _ = slip.ReadString("(defun bad ((t 0)) nil)").Eval(slip.NewScope()) })
	require.Panics(t, func() { _ = slip.ReadString("(defun bad (t) nil)").Eval(slip.NewScope()) })
}

func TestDefunAgain(t *testing.T) {
	var out bytes.Buffer
	orig := slip.ErrorOutput
	defer func() { slip.ErrorOutput = orig }()
	slip.ErrorOutput = &slip.OutputStream{Writer: &out}
	code := slip.ReadString(`
(defun again () 3)
(defun again () 7)
(again)`)
	scope := slip.NewScope()
	require.Equal(t, slip.Fixnum(7), code.Eval(scope))
	require.Equal(t, "WARNING: redefining COMMON-LISP-USER::AGAIN in DEFUN\n", out.String())
}

func TestDefunTooManyArgs(t *testing.T) {
	code := slip.ReadString(`
(defun too-many () 3)
(too-many 7)`)
	scope := slip.NewScope()
	require.Panics(t, func() { _ = code.Eval(scope) })
}

func TestDefunTooFewArgs(t *testing.T) {
	code := slip.ReadString(`
(defun too-few (x) x)
(too-few)`)
	scope := slip.NewScope()
	require.Panics(t, func() { _ = code.Eval(scope) })
}

func TestDefunLocked(t *testing.T) {
	code := slip.ReadString("(defun setq () nil)")
	scope := slip.NewScope()
	require.Panics(t, func() { _ = code.Eval(scope) })
}

// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
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

func TestDefunPre(t *testing.T) {
	code := slip.ReadString(`
(defun aaa () (bbb))
(defun bbb () 7)
(aaa)`)
	scope := slip.NewScope()
	code.Compile()
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

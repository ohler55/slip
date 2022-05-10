// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestLetEmpty(t *testing.T) {
	code := slip.ReadString("(let ())")
	scope := slip.NewScope()
	require.Equal(t, nil, code.Eval(scope))
}

func TestLetNoInitialForm(t *testing.T) {
	code := slip.ReadString("(let (x (y)) (list x y))")
	scope := slip.NewScope()
	require.Equal(t, "(nil nil)", slip.ObjectString(code.Eval(scope)))
}

func TestLetParallel(t *testing.T) {
	code := slip.ReadString("(setq x 3) (let ((x 4) (y x)) (list x y))")
	scope := slip.NewScope()
	require.Equal(t, "(4 3)", slip.ObjectString(code.Eval(scope)))
}

func TestLetNoBindings(t *testing.T) {
	code := slip.ReadString("(let)")
	require.Panics(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestLetBadBindings(t *testing.T) {
	code := slip.ReadString("(let t)")
	require.Panics(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestLetEmptyBinding(t *testing.T) {
	code := slip.ReadString("(let (()))")
	require.Panics(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestLetBadBindingVar(t *testing.T) {
	code := slip.ReadString("(let ((t 3)))")
	require.Panics(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestLetBadVar(t *testing.T) {
	code := slip.ReadString("(let (t))")
	require.Panics(t, func() { _ = code.Eval(slip.NewScope()) })
}

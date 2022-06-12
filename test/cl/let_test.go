// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestLetEmpty(t *testing.T) {
	code := slip.ReadString("(let ())")
	scope := slip.NewScope()
	tt.Equal(t, nil, code.Eval(scope))
}

func TestLetNoInitialForm(t *testing.T) {
	code := slip.ReadString("(let (x (y)) (list x y))")
	scope := slip.NewScope()
	tt.Equal(t, "(nil nil)", slip.ObjectString(code.Eval(scope)))
}

func TestLetNested(t *testing.T) {
	code := slip.ReadString("(let ((x 1)) (let ((y 2)) (list x y)))")
	scope := slip.NewScope()
	tt.Equal(t, "(1 2)", slip.ObjectString(code.Eval(scope)))
}

func TestLetParallel(t *testing.T) {
	code := slip.ReadString("(setq x 3) (let ((x 4) (y x)) (list x y))")
	scope := slip.NewScope()
	tt.Equal(t, "(4 3)", slip.ObjectString(code.Eval(scope)))
}

func TestLetNoBindings(t *testing.T) {
	code := slip.ReadString("(let)")
	tt.Panic(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestLetBadBindings(t *testing.T) {
	code := slip.ReadString("(let t)")
	tt.Panic(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestLetEmptyBinding(t *testing.T) {
	code := slip.ReadString("(let (()))")
	tt.Panic(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestLetBadBindingVar(t *testing.T) {
	code := slip.ReadString("(let ((t 3)))")
	tt.Panic(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestLetBadVar(t *testing.T) {
	code := slip.ReadString("(let (t))")
	tt.Panic(t, func() { _ = code.Eval(slip.NewScope()) })
}

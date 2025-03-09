// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLetxEmpty(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(let* ())", scope)
	tt.Equal(t, nil, code.Eval(scope, nil))
}

func TestLetxNoInitialForm(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(let* (x (y)) (list x y))", scope)
	tt.Equal(t, "(nil nil)", slip.ObjectString(code.Eval(scope, nil)))
}

func TestLetxParallel(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(setq x 3) (let* ((x 4) (y x)) (list x y))", scope)
	tt.Equal(t, "(4 4)", slip.ObjectString(code.Eval(scope, nil)))
}

func TestLetxNoBindings(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(let*)", scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestLetxBadBindings(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(let* t)", scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestLetxEmptyBinding(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(let* (()))", scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestLetxBadBindingVar(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(let* ((t 3)))", scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestLetxBadVar(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(let* (t))", scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestLetxReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (let* ((x 1) (y 2)) (return 7) (list x y)))`,
		Expect: "7",
	}).Test(t)
}

func TestLetxGo(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (z)
                  (tagbody
                   (let* ((x 1) (y 2))
                    (setq z (list x y))
                    (go skip)
                    (setq z 0))
                   skip)
                  z)`,
		Expect: "(1 2)",
	}).Test(t)
}

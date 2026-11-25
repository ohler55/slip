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
	tt.Equal(t, nil, slip.ReadEvalString("(let* ())", scope))
}

func TestLetxNoInitialForm(t *testing.T) {
	scope := slip.NewScope()
	tt.Equal(t, "(nil nil)", slip.ObjectString(slip.ReadEvalString("(let* (x (y)) (list x y))", scope)))
}

func TestLetxPreProv(t *testing.T) {
	orig := slip.Provenance
	slip.Provenance = true
	defer func() { slip.Provenance = orig }()
	scope := slip.NewScope()
	tt.Equal(t, "(nil nil)", slip.ObjectString(slip.ReadEvalString("(let* (x (y)) (list x y))", scope)))
}

func TestLetxParallel(t *testing.T) {
	scope := slip.NewScope()
	tt.Equal(t, "(4 4)", slip.ObjectString(slip.ReadEvalString("(setq x 3) (let* ((x 4) (y x)) (list x y))", scope)))
}

func TestLetxNoBindings(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let*)", scope) })
}

func TestLetxBadBindings(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let* t)", scope) })
}

func TestLetxEmptyBinding(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let* (()))", scope) })
}

func TestLetxBadBindingVar(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let* ((t 3)))", scope) })
}

func TestLetxBadVar(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let* (t))", scope) })
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
